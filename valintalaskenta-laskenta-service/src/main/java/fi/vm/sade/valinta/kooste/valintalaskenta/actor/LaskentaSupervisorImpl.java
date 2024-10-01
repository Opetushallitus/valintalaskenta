package fi.vm.sade.valinta.kooste.valintalaskenta.actor;

import java.util.Arrays;
import java.util.Optional;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

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
  private final SeurantaDao seurantaDao;

  private final int maxWorkers;
  private final ExecutorService executorService;

  @Autowired
  public LaskentaSupervisorImpl(
      LaskentaActorFactory laskentaActorFactory,
      SeurantaDao seurantaDao,
      @Value("${valintalaskentakoostepalvelu.maxWorkerCount:8}") int maxWorkers) {
    this.laskentaActorFactory = laskentaActorFactory;
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

  private void startLaskenta(LaskentaDto laskenta) {
    LOG.info("Luodaan ja aloitetaan Laskenta uuid:lle {}", laskenta.getUuid());

    try {
      LaskentaActor laskentaActor = laskentaActorFactory.createLaskentaActor(laskenta);
      this.executorService.submit(() -> {
        try {
          laskentaActor.start();
        } catch(Throwable t) {
          cancelLaskenta("Laskenta päättyi virheeseen", Optional.of(t), laskenta.getUuid());
        }
      });
    } catch (Throwable t) {
      this.cancelLaskenta("\r\n###\r\n### Laskenta uuid:lle {} haulle {} ei kaynnistynyt!\r\n###", Optional.of(t), laskenta.getUuid());
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
