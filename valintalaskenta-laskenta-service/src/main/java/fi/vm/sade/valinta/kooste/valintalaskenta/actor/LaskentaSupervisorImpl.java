package fi.vm.sade.valinta.kooste.valintalaskenta.actor;

import java.util.Collection;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import fi.vm.sade.valintalaskenta.laskenta.dao.SeurantaDao;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;

@Service
public class LaskentaSupervisorImpl {
  private static final Logger LOG = LoggerFactory.getLogger(LaskentaSupervisorImpl.class);

  private final LaskentaActorFactory laskentaActorFactory;
  private final SeurantaDao seurantaDao;

  private final int maxYhtaaikaisetHakukohteet;

  private final String noodiId;

  private final Executor executor;

  @Autowired
  public LaskentaSupervisorImpl(
      LaskentaActorFactory laskentaActorFactory,
      SeurantaDao seurantaDao,
      @Value("${valintalaskentakoostepalvelu.maxWorkerCount:8}") int maxWorkers) { // TODO: tämä pitää laittaa kantaan
    this.laskentaActorFactory = laskentaActorFactory;
    this.seurantaDao = seurantaDao;
    this.maxYhtaaikaisetHakukohteet = maxWorkers;
    this.noodiId = UUID.randomUUID().toString();
    this.executor = Executors.newWorkStealingPool(8);
  }

  @Scheduled(initialDelay = 15, fixedDelay = 15, timeUnit = TimeUnit.SECONDS)
  public void merkkaaNoodiLiveksi() {
    LOG.debug("Merkataan noodi " + this.noodiId + " liveksi");
    this.seurantaDao.merkkaaNoodiLiveksi(this.noodiId);
  }

  @Scheduled(initialDelay = 15, fixedDelay = 15, timeUnit = TimeUnit.SECONDS)
  public void resetoiKuolleidenNoodienLaskennat() {
    LOG.debug("Resetoidaan kuolleiden noodien laskennat");
    this.seurantaDao.resetoiKuolleidenNoodienLaskennat(60);
  }

  @Scheduled(initialDelay = 15, fixedDelay = 5, timeUnit = TimeUnit.SECONDS)
  public void fetchAndStartHakukohde() {
    this.executor.execute(() -> {
      try {
        LOG.debug("Käynnistetään hakukohteiden laskennat");
        while(true) {
          Optional<ImmutablePair<UUID, Collection<String>>> hakukohteet = this.seurantaDao.otaSeuraavatHakukohteetTyonAlle(this.noodiId, this.maxYhtaaikaisetHakukohteet);
          if(!hakukohteet.isPresent()) {
            LOG.debug("Ei käynnistettäviä hakukohteita");
            return;
          }
          UUID uuid = hakukohteet.get().getLeft();
          Collection<String> hakukohdeOids = hakukohteet.get().getRight();
          LOG.info("Käynnistetään laskennan {} hakukohteiden {} laskenta", uuid, hakukohdeOids.stream().collect(Collectors.joining(", ")));

          laskentaActorFactory.suoritaLaskentaHakukohteille(this.seurantaDao.haeLaskenta(uuid.toString()).get(), hakukohdeOids)
              .thenRunAsync(() -> this.seurantaDao.merkkaaHakukohteetValmiiksi(uuid, hakukohdeOids))
              .exceptionallyAsync(t -> {
                String msg = "Laskennan %s hakukohteen %s laskenta päättyi virheeseen";
                LOG.error(msg, t);
                this.seurantaDao.merkkaaHakukohteetEpaonnistuneeksi(uuid, hakukohdeOids, 2, msg);
                return null;
              });
        }
      } catch(Throwable t) {
        LOG.error("Virhe hakukohteen laskennan käynnistämisessä", t);
      }
    });
  }
}
