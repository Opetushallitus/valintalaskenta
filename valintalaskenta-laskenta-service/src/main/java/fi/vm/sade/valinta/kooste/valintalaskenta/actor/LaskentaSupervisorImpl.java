package fi.vm.sade.valinta.kooste.valintalaskenta.actor;

import java.util.Arrays;
import java.util.Optional;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import fi.vm.sade.valinta.kooste.valintalaskenta.service.LaskentaParameterService;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.HakukohdeTila;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.IlmoitusDto;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.LaskentaDto;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.LaskentaTila;
import fi.vm.sade.valintalaskenta.laskenta.dao.SeurantaDao;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.jmx.export.annotation.ManagedResource;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;

@Service
@ManagedResource(
    objectName = "OPH:name=LaskentaActorSystem",
    description = "LaskentaActorSystem mbean")
public class LaskentaSupervisorImpl {
  private static final Logger LOG = LoggerFactory.getLogger(LaskentaSupervisorImpl.class);

  private final LaskentaActorFactory laskentaActorFactory;
  private final LaskentaParameterService laskentaParameterService;
  private final SeurantaDao seurantaDao;

  private final int maxWorkers;
  private final ExecutorService executorService;

  @Autowired
  public LaskentaSupervisorImpl(
      LaskentaParameterService laskentaParameterService,
      LaskentaActorFactory laskentaActorFactory,
      SeurantaDao seurantaDao,
      @Value("${valintalaskentakoostepalvelu.maxWorkerCount:8}") int maxWorkers) {
    this.laskentaActorFactory = laskentaActorFactory;
    this.laskentaParameterService = laskentaParameterService;
    this.seurantaDao = seurantaDao;
    this.maxWorkers = maxWorkers;
    this.executorService =
        Executors.newWorkStealingPool(
            Math.max(Runtime.getRuntime().availableProcessors(), maxWorkers));
  }

  @Scheduled(initialDelay = 15, fixedDelay = 5, timeUnit = TimeUnit.SECONDS)
  public void fetchAndStartLaskenta() {
    try {
      while(true) {
        if(this.seurantaDao.haeKaynnissaOlevatLaskennat().size()>=this.maxWorkers) {
          LOG.info("Maksimäärä laskentoja käynnissä");
          return;
        }
        Optional<LaskentaDto> laskenta = seurantaDao.otaSeuraavaLaskentaTyonAlle();
        if(!laskenta.isPresent()) {
          LOG.info("Ei laskettavaa");
          return;
        }
        this.startLaskenta(laskenta);
      }
    } catch(Throwable t) {
      LOG.error("Virhe laskennan käynnistämisessä", t);
    }
  }

  private void startLaskenta(Optional<LaskentaDto> laskenta) {
    LOG.info("Luodaan ja aloitetaan Laskenta uuid:lle {}", laskenta.get().getUuid());

    LaskentaActorParams params;
    try {
      params = laskentaParameterService.fetchLaskentaParams(laskenta.get());
    } catch (Throwable t) {
      cancelLaskenta(
          "Taustatietojen haku epäonnistui",
          Optional.of(t),
          laskenta.get().getUuid());
      return;
    }

    if(params.getHakukohdeOids().isEmpty()) {
      cancelLaskenta(
          "Haulla "
              + laskenta.get().getUuid()
              + " ei saatu hakukohteita! Onko valinnat synkronoitu tarjonnan kanssa?",
          null,
          laskenta.get().getUuid());
      return;
    }

    try {
      LaskentaActor laskentaActor = laskentaActorFactory.createLaskentaActor(params);
      this.executorService.submit(() -> {
        try {
          laskentaActor.start();
        } catch(Throwable t) {
          cancelLaskenta("Laskenta päättyi virheeseen", Optional.of(t), laskenta.get().getUuid());
        }
      });
    } catch (Exception e) {
      this.cancelLaskenta("\r\n###\r\n### Laskenta uuid:lle {} haulle {} ei kaynnistynyt!\r\n###", Optional.of(e), params.getUuid());
    }
  }

  private void cancelLaskenta(String msg, Optional<Throwable> t, String uuid) {
    if (t.isPresent()) LOG.error(msg, t);
    else LOG.error(msg);
    LaskentaTila tila = LaskentaTila.VALMIS;
    HakukohdeTila hakukohdetila = HakukohdeTila.KESKEYTETTY;
    Optional<IlmoitusDto> ilmoitusDtoOptional =
        t.map(
            poikkeus -> IlmoitusDto.virheilmoitus(msg, Arrays.toString(poikkeus.getStackTrace())));

    seurantaDao.merkkaaTila(uuid, tila, hakukohdetila, ilmoitusDtoOptional);
  }
}
