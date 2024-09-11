package fi.vm.sade.valinta.kooste.valintalaskenta.actor;

import static fi.vm.sade.valinta.kooste.valintalaskenta.actor.laskenta.LaskentaStarterActor.*;

import akka.actor.ActorRef;
import akka.actor.ActorSystem;
import akka.actor.TypedActor;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.typesafe.config.ConfigFactory;
import fi.vm.sade.valinta.kooste.external.resource.ohjausparametrit.dto.ParametritDTO;
import fi.vm.sade.valinta.kooste.external.resource.seuranta.LaskentaSeurantaAsyncResource;
import fi.vm.sade.valinta.kooste.external.resource.tarjonta.Haku;
import fi.vm.sade.valinta.kooste.external.resource.valintatulosservice.dto.AuditSession;
import fi.vm.sade.valinta.kooste.valintalaskenta.dto.Laskenta;
import fi.vm.sade.valinta.kooste.valintalaskenta.dto.LaskentaStartParams;
import fi.vm.sade.valinta.kooste.valintalaskenta.route.ValintalaskentaKerrallaRoute;
import fi.vm.sade.valinta.kooste.valintalaskenta.route.ValintalaskentaKerrallaRouteValvomo;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.TimeUnit;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.jmx.export.annotation.ManagedOperation;
import org.springframework.jmx.export.annotation.ManagedResource;
import org.springframework.stereotype.Service;
import scala.concurrent.duration.FiniteDuration;

@Service
@ManagedResource(
    objectName = "OPH:name=LaskentaActorSystem",
    description = "LaskentaActorSystem mbean")
public class LaskentaActorSystem
    implements ValintalaskentaKerrallaRouteValvomo,
        ValintalaskentaKerrallaRoute,
        LaskentaSupervisor,
        ApplicationListener<ContextRefreshedEvent> {
  private static final Logger LOG = LoggerFactory.getLogger(LaskentaActorSystem.class);

  private final LaskentaActorFactory laskentaActorFactory;

  private final LaskentaSeurantaAsyncResource seurantaAsyncResource;
  private final ActorSystem actorSystem;
  private final ActorRef laskennanKaynnistajaActor;
  private final Map<String, LaskentaActorWrapper> runningLaskentas = Maps.newConcurrentMap();
  private final LaskentaStarter laskentaStarter;

  @Autowired
  public LaskentaActorSystem(
      LaskentaSeurantaAsyncResource seurantaAsyncResource,
      LaskentaStarter laskentaStarter,
      LaskentaActorFactory laskentaActorFactory,
      @Value("${valintalaskentakoostepalvelu.maxWorkerCount:8}") int maxWorkers) {
    this.laskentaActorFactory = laskentaActorFactory;
    this.laskentaStarter = laskentaStarter;
    this.seurantaAsyncResource = seurantaAsyncResource;
    this.actorSystem =
        ActorSystem.create("ValintalaskentaActorSystem", ConfigFactory.defaultOverrides());
    laskennanKaynnistajaActor = actorSystem.actorOf(props(this, maxWorkers));
  }

  @ManagedOperation
  public void resetActorCounter() {
    laskennanKaynnistajaActor.tell(new ResetWorkerCount(), ActorRef.noSender());
  }

  @ManagedOperation
  public void setMaxWorkerCount(int maxWorkerCount) {
    laskennanKaynnistajaActor.tell(new MaxWorkerCount(maxWorkerCount), ActorRef.noSender());
  }

  @ManagedOperation
  public void startAllWorkers() {
    laskennanKaynnistajaActor.tell(new StartAllWorkers(), ActorRef.noSender());
  }

  @Override
  public void workAvailable() {
    laskennanKaynnistajaActor.tell(new WorkAvailable(), ActorRef.noSender());
  }

  @Override
  public void onApplicationEvent(ContextRefreshedEvent event) {
    laskennanKaynnistajaActor.tell(new StartAllWorkers(), ActorRef.noSender());
  }

  @Override
  public void suoritaValintalaskentaKerralla(
      final Haku haku,
      final ParametritDTO parametritDTO,
      final LaskentaStartParams laskentaStartParams) {
    AuditSession auditSession = laskentaStartParams.getAuditSession();
    LaskentaActor laskentaActor =
        laskentaActorFactory.createLaskentaActor(
            auditSession, this, haku, new LaskentaActorParams(laskentaStartParams, parametritDTO));
    startLaskentaActor(laskentaStartParams, laskentaActor);
  }

  @Override
  public List<Laskenta> runningLaskentas() {
    return Lists.newArrayList(runningLaskentas.values());
  }

  @Override
  public Optional<Laskenta> fetchLaskenta(String uuid) {
    return Optional.ofNullable(runningLaskentas.get(uuid));
  }

  @Override
  public void ready(String uuid) {
    LOG.trace("Ilmoitettu actor valmiiksi laskennalle (" + uuid + ")");
    LaskentaActorWrapper actorWrapper = runningLaskentas.remove(uuid);
    if (actorWrapper == null) {
      throw new IllegalStateException(
          "Ei löytynyt valmiiksi merkattavaa actoria laskennalle " + uuid);
    }
    stopActor(uuid, actorWrapper.laskentaActor());
  }

  public void fetchAndStartLaskenta() {
    seurantaAsyncResource
        .otaSeuraavaLaskentaTyonAlle()
        .subscribe(
            this::startLaskentaIfWorkAvailable,
            (Throwable t) -> {
              LOG.warn("Uutta laskentaa ei saatu tyon alle seurannasta. Yritetään uudelleen.", t);
              _fetchAndStartLaskentaRetry(); // FIXME kill me OK-152
            });
  }

  private void _fetchAndStartLaskentaRetry() {
    seurantaAsyncResource
        .otaSeuraavaLaskentaTyonAlle()
        .subscribe(
            this::startLaskentaIfWorkAvailable,
            (Throwable t) -> {
              String message = "Uutta laskentaa ei saatu tyon alle seurannasta.";
              LOG.error(message, t);
              actorSystem
                  .scheduler()
                  .scheduleOnce(
                      FiniteDuration.create(5, TimeUnit.SECONDS),
                      laskennanKaynnistajaActor,
                      new WorkerAvailable(),
                      actorSystem.dispatcher(),
                      ActorRef.noSender());
              throw new RuntimeException(message, t);
            });
  }

  private void startLaskentaIfWorkAvailable(Optional<String> uuid) {
    if (!uuid.isPresent()) {
      LOG.trace("Ei laskettavaa");
      laskennanKaynnistajaActor.tell(new NoWorkAvailable(), ActorRef.noSender());
    } else {
      LOG.info("Luodaan ja aloitetaan Laskenta uuid:lle {}", uuid.get());
      laskentaStarter.fetchLaskentaParams(
          laskennanKaynnistajaActor,
          uuid.get(),
          (haku, params) ->
              startLaskentaActor(
                  params.getLaskentaStartParams(),
                  laskentaActorFactory.createLaskentaActor(
                      params.getLaskentaStartParams().getAuditSession(), this, haku, params)));
    }
  }

  protected void startLaskentaActor(LaskentaStartParams params, LaskentaActor laskentaActor) {
    String uuid = params.getUuid();
    String hakuOid = params.getHakuOid();

    runningLaskentas.merge(
        uuid,
        new LaskentaActorWrapper(params, laskentaActor),
        (LaskentaActorWrapper oldValue, LaskentaActorWrapper value) -> {
          LOG.warn(
              "\r\n###\r\n### Laskenta uuid:lle {} haulle {} oli jo kaynnissa! Lopetataan vanha laskenta!\r\n###",
              uuid,
              hakuOid);
          stopActor(uuid, oldValue.laskentaActor());
          return value;
        });

    try {
      laskentaActor.start();
    } catch (Exception e) {
      runningLaskentas.remove(uuid);
      LOG.error(
          "\r\n###\r\n### Laskenta uuid:lle {} haulle {} ei kaynnistynyt!\r\n###",
          uuid,
          hakuOid,
          e);
    }
  }

  private void stopActor(String uuid, LaskentaActor actor) {
    LOG.trace("Pysaytetaan actor laskennalle (" + uuid + ")");
    laskennanKaynnistajaActor.tell(new WorkerAvailable(), ActorRef.noSender());
    if (actor != null) {
      try {
        TypedActor.get(actorSystem).poisonPill(actor);
        LOG.trace("PoisonPill lahetetty onnistuneesti Actorille " + uuid);
      } catch (Exception e) {
        LOG.error("PoisonPill lahetys epaonnistui Actorille " + uuid, e);
      }
    } else {
      LOG.warn("Yritettiin sammuttaa laskenta " + uuid + ", mutta laskenta ei ollut enaa ajossa!");
    }
  }
}
