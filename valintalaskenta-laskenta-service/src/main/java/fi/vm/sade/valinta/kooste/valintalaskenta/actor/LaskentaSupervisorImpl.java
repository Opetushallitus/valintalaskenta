package fi.vm.sade.valinta.kooste.valintalaskenta.actor;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Optional;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import fi.vm.sade.valinta.kooste.AuditSession;
import fi.vm.sade.valinta.kooste.valintalaskenta.dto.HakukohdeJaOrganisaatio;
import fi.vm.sade.valinta.kooste.valintalaskenta.dto.LaskentaStartParams;
import fi.vm.sade.valinta.kooste.valintalaskenta.service.HakukohdeService;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.HakukohdeTila;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.IlmoitusDto;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.LaskentaDto;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.LaskentaTila;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.LaskentaTyyppi;
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
  private final HakukohdeService hakukohdeService;
  private final SeurantaDao seurantaDao;

  private final int maxWorkers;
  private final ExecutorService executorService;

  @Autowired
  public LaskentaSupervisorImpl(
      HakukohdeService hakukohdeService,
      LaskentaActorFactory laskentaActorFactory,
      SeurantaDao seurantaDao,
      @Value("${valintalaskentakoostepalvelu.maxWorkerCount:8}") int maxWorkers) {
    this.laskentaActorFactory = laskentaActorFactory;
    this.hakukohdeService = hakukohdeService;
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
        this.startLaskenta(laskenta.get());
      }
    } catch(Throwable t) {
      LOG.error("Virhe laskennan käynnistämisessä", t);
    }
  }

  private static AuditSession koosteAuditSession(LaskentaDto laskenta) {
    final String userAgent = "-";
    final String inetAddress = "127.0.0.1";
    AuditSession auditSession =
        new AuditSession(laskenta.getUserOID(), Collections.emptyList(), userAgent, inetAddress);
    auditSession.setSessionId(laskenta.getUuid());
    auditSession.setPersonOid(laskenta.getUserOID());
    return auditSession;
  }

  private void startLaskenta(LaskentaDto laskenta) {
    LOG.info("Luodaan ja aloitetaan Laskenta uuid:lle {}", laskenta.getUuid());

    LaskentaStartParams laskentaStartParams = new LaskentaStartParams(
        koosteAuditSession(laskenta),
        laskenta.getUuid(),
        laskenta.getHakuOid(),
        laskenta.isErillishaku(),
        true,
        LaskentaTyyppi.VALINTARYHMA.equals(laskenta.getTyyppi()),
        laskenta.getValinnanvaihe(),
        laskenta.getValintakoelaskenta(),
        laskenta.getTyyppi());

    Collection<HakukohdeJaOrganisaatio> hakukohteet;
    try {
      hakukohteet = hakukohdeService.fetchHakukohteet(laskenta);
    } catch (Throwable t) {
      cancelLaskenta(
          "Taustatietojen haku epäonnistui",
          Optional.of(t),
          laskenta.getUuid());
      return;
    }

    if(hakukohteet.isEmpty()) {
      cancelLaskenta(
          "Haulla "
              + laskenta.getUuid()
              + " ei saatu hakukohteita! Onko valinnat synkronoitu tarjonnan kanssa?",
          null,
          laskenta.getUuid());
      return;
    }

    try {
      LaskentaActor laskentaActor = laskentaActorFactory.createLaskentaActor(laskentaStartParams, hakukohteet);
      this.executorService.submit(() -> {
        try {
          laskentaActor.start();
        } catch(Throwable t) {
          cancelLaskenta("Laskenta päättyi virheeseen", Optional.of(t), laskenta.getUuid());
        }
      });
    } catch (Exception e) {
      this.cancelLaskenta("\r\n###\r\n### Laskenta uuid:lle {} haulle {} ei kaynnistynyt!\r\n###", Optional.of(e), laskentaStartParams.getUuid());
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
