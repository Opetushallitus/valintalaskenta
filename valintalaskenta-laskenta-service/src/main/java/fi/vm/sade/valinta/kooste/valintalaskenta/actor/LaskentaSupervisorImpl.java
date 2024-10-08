package fi.vm.sade.valinta.kooste.valintalaskenta.actor;

import java.util.Collection;
import java.util.Optional;
import java.util.UUID;
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
import org.springframework.transaction.support.TransactionTemplate;

@Service
public class LaskentaSupervisorImpl {
  private static final Logger LOG = LoggerFactory.getLogger(LaskentaSupervisorImpl.class);

  private final TransactionTemplate transactionTemplate;
  private final LaskentaActorFactory laskentaActorFactory;
  private final SeurantaDao seurantaDao;

  private final int maxYhtaaikaisetHaut;
  private final int maxYhtaaikaisetHakukohteet;

  @Autowired
  public LaskentaSupervisorImpl(
      TransactionTemplate transactionTemplate,
      LaskentaActorFactory laskentaActorFactory,
      SeurantaDao seurantaDao,
      @Value("${valintalaskentakoostepalvelu.maxWorkerCount:8}") int maxWorkers) { // TODO: tämä pitää laittaa kantaan
    this.transactionTemplate = transactionTemplate;
    this.laskentaActorFactory = laskentaActorFactory;
    this.seurantaDao = seurantaDao;
    this.maxYhtaaikaisetHaut = maxWorkers;
    this.maxYhtaaikaisetHakukohteet = maxWorkers;
  }

  @Scheduled(initialDelay = 15, fixedDelay = 5, timeUnit = TimeUnit.SECONDS)
  public void fetchAndStartHakukohde() {
    try {
      while(true) {
        if(this.seurantaDao.haeKaynnissaOlevienHakukohteidenMaara()>=this.maxYhtaaikaisetHakukohteet) {
          LOG.info("Maksimäärä hakukohteita käynnissä");
          return;
        }
        Optional<ImmutablePair<UUID, Collection<String>>> hakukohteet = this.transactionTemplate.execute(ts ->
          this.seurantaDao.otaSeuraavatHakukohteetTyonAlle());

        if(!hakukohteet.isPresent()) {
          LOG.info("Ei käynnistettäviä hakukohteita");
          return;
        }
        UUID uuid = hakukohteet.get().getLeft();
        Collection<String> hakukohdeOids = hakukohteet.get().getRight();
        LOG.info("Käynnistetään laskennan {} hakukohteiden {} laskenta", uuid, hakukohdeOids.stream().collect(Collectors.joining(", ")));

        laskentaActorFactory.suoritaLaskentaHakukohteille(this.seurantaDao.haeLaskenta(uuid.toString()).get(), hakukohdeOids)
          .thenRunAsync(() ->
            this.transactionTemplate.executeWithoutResult(ts -> this.seurantaDao.merkkaaHakukohteetValmiiksi(uuid, hakukohdeOids)))
          .exceptionallyAsync(t -> {
            String msg = "Laskennan %s hakukohteen %s laskenta päättyi virheeseen";
            LOG.error(msg, t);
            this.transactionTemplate.executeWithoutResult(ts -> this.seurantaDao.merkkaaHakukohteetEpaonnistuneeksi(uuid, hakukohdeOids, 1, msg));
            return null;
          });
      }
    } catch(Throwable t) {
      LOG.error("Virhe hakukohteen laskennan käynnistämisessä", t);
    }
  }
}
