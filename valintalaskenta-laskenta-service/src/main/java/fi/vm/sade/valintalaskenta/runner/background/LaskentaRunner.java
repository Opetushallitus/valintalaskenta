package fi.vm.sade.valintalaskenta.runner.background;

import fi.vm.sade.valintalaskenta.laskenta.dao.SeurantaDao;
import fi.vm.sade.valintalaskenta.runner.service.EcsTaskManager;
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

/** Luokka joka sisältää laskentojen ajotoiminnallisuuden. */
@Service
public class LaskentaRunner {
  private static final Logger LOG = LoggerFactory.getLogger(LaskentaRunner.class);

  private final SuoritaLaskentaService suoritaLaskentaService;
  private final SeurantaDao seurantaDao;

  private final String noodiId;
  private final Executor executor;

  @Autowired
  public LaskentaRunner(
      SuoritaLaskentaService suoritaLaskentaService,
      SeurantaDao seurantaDao,
      EcsTaskManager ecsTaskManager) {
    this.suoritaLaskentaService = suoritaLaskentaService;
    this.seurantaDao = seurantaDao;
    this.noodiId = ecsTaskManager.getTaskArn();
    this.executor = Executors.newWorkStealingPool(256);
  }

  /**
   * Merkataan noodi kantaan säännöllisesti liveksi (päivitetään timestamp jolloin noodi ollu
   * elossa), jolloin kuolleiden noodien hakukohteiden resetointitoiminnallisuus ei resetoi tämän
   * noodin hakukohteita.
   *
   * <p>Lisäksi merkataan mahdollisten kuolleiden noodien (eivät ole merkanneet itseään liveksi
   * minuuttiin) hakukohteet suorittamattomiksi
   */
  @Scheduled(initialDelay = 15, fixedDelay = 15, timeUnit = TimeUnit.SECONDS)
  public void resetoiKuolleidenNoodienLaskennat() {
    this.executor.execute(
        () -> {
          LOG.debug("Merkataan noodi " + this.noodiId + " liveksi");
          this.seurantaDao.merkkaaNoodiLiveksi(this.noodiId);

          LOG.debug("Resetoidaan kuolleiden noodien laskennat");
          this.seurantaDao.resetoiKuolleidenNoodienLaskennat(60);
        });
  }

  /**
   * Aloitetaan lasketaan uusia hakukohteita jos mahdollista (tarjolla pitää olla laskemattomia
   * hakukohteita ja tällä noodilla pitää olla tilaa).
   */
  @Scheduled(initialDelay = 15, fixedDelay = 5, timeUnit = TimeUnit.SECONDS)
  public void runFetchAndStartHakukohteet() {
    this.executor.execute(() -> this.fetchAndStartHakukohteet());
  }

  public void fetchAndStartHakukohteet() {
    this.executor.execute(
        () -> {
          try {
            LOG.debug("Käynnistetään hakukohteiden laskennat");
            int maxYhtaaikaisetHakukohteet =
                Integer.parseInt(this.seurantaDao.lueParametri("maxYhtaaikaisetHakukohteet"));
            int liveNoodienMaara = this.seurantaDao.liveNoodienMaara(60);
            int rinnakkaisuus = maxYhtaaikaisetHakukohteet / Math.max(1, liveNoodienMaara);

            while (true) {
              Optional<ImmutablePair<UUID, Collection<String>>> hakukohteet =
                  this.seurantaDao.otaSeuraavatHakukohteetTyonAlle(this.noodiId, rinnakkaisuus);
              if (!hakukohteet.isPresent()) {
                LOG.debug("Ei käynnistettäviä hakukohteita");
                break;
              }

              this.executor.execute(
                  () -> {
                    UUID uuid = hakukohteet.get().getLeft();
                    Collection<String> hakukohdeOids = hakukohteet.get().getRight();
                    LOG.info(
                        "Käynnistetään laskennan {} hakukohteiden {} laskenta",
                        uuid,
                        hakukohdeOids.stream().collect(Collectors.joining(", ")));

                    try {
                      suoritaLaskentaService.suoritaLaskentaHakukohteille(
                          this.seurantaDao.haeLaskenta(uuid.toString()).get(),
                          hakukohdeOids,
                          rinnakkaisuus);
                      this.seurantaDao.merkkaaHakukohteetValmiiksi(uuid, hakukohdeOids);
                    } catch (Throwable t) {
                      LOG.error(
                          String.format(
                              "Laskennan %s hakukohteiden %s laskenta päättyi virheeseen",
                              uuid, hakukohdeOids.stream().collect(Collectors.joining(", "))),
                          t);
                      this.seurantaDao.merkkaaHakukohteetEpaonnistuneeksi(
                          uuid, hakukohdeOids, 2, getUnderlyingCause(t).getMessage());
                    }
                  });
            }
          } catch (Throwable t) {
            LOG.error("Virhe hakukohteen laskennan käynnistämisessä", t);
          }
        });
  }

  private static Throwable getUnderlyingCause(Throwable t) {
    if (t.getCause() != null) {
      return getUnderlyingCause(t.getCause());
    }
    return t;
  }
}
