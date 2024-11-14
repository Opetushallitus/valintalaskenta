package fi.vm.sade.valintalaskenta.runner.background;

import fi.vm.sade.valintalaskenta.laskenta.dao.SeurantaDao;
import fi.vm.sade.valintalaskenta.runner.service.SuoritaLaskentaService;
import java.util.Collection;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;

@Service
public class LaskentaRunner {
  private static final Logger LOG = LoggerFactory.getLogger(LaskentaRunner.class);

  private final SuoritaLaskentaService suoritaLaskentaService;
  private final SeurantaDao seurantaDao;

  private final String noodiId;

  private final Executor executor;

  @Autowired
  public LaskentaRunner(SuoritaLaskentaService suoritaLaskentaService, SeurantaDao seurantaDao) {
    this.suoritaLaskentaService = suoritaLaskentaService;
    this.seurantaDao = seurantaDao;
    this.noodiId = UUID.randomUUID().toString();
    this.executor = Executors.newWorkStealingPool(64);
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

  private static Throwable getUnderlyingCause(Throwable t) {
    if (t.getCause() != null) {
      return getUnderlyingCause(t.getCause());
    }
    return t;
  }

  @Scheduled(initialDelay = 15, fixedDelay = 5, timeUnit = TimeUnit.SECONDS)
  public void fetchAndStartHakukohde() {
    this.executor.execute(
        () -> {
          try {
            LOG.debug("Käynnistetään hakukohteiden laskennat");
            int maxYhtaaikaisetHakukohteet =
                Integer.parseInt(this.seurantaDao.lueParametri("maxYhtaaikaisetHakukohteet"));

            while (true) {
              Optional<ImmutablePair<UUID, Collection<String>>> hakukohteet =
                  this.seurantaDao.otaSeuraavatHakukohteetTyonAlle(
                      this.noodiId, maxYhtaaikaisetHakukohteet);
              if (!hakukohteet.isPresent()) {
                LOG.debug("Ei käynnistettäviä hakukohteita");
                return;
              }
              UUID uuid = hakukohteet.get().getLeft();
              Collection<String> hakukohdeOids = hakukohteet.get().getRight();
              LOG.info(
                  "Käynnistetään laskennan {} hakukohteiden {} laskenta",
                  uuid,
                  hakukohdeOids.stream().collect(Collectors.joining(", ")));

              suoritaLaskentaService
                  .suoritaLaskentaHakukohteille(
                      this.seurantaDao.haeLaskenta(uuid.toString()).get(), hakukohdeOids)
                  .thenRunAsync(
                      () -> this.seurantaDao.merkkaaHakukohteetValmiiksi(uuid, hakukohdeOids))
                  .exceptionallyAsync(
                      t -> {
                        LOG.error(
                            String.format(
                                "Laskennan %s hakukohteiden %s laskenta päättyi virheeseen",
                                uuid, hakukohdeOids.stream().collect(Collectors.joining(", "))),
                            t);
                        this.seurantaDao.merkkaaHakukohteetEpaonnistuneeksi(
                            uuid, hakukohdeOids, 2, getUnderlyingCause(t).getMessage());
                        return null;
                      });
            }
          } catch (Throwable t) {
            LOG.error("Virhe hakukohteen laskennan käynnistämisessä", t);
          }
        });
  }
}
