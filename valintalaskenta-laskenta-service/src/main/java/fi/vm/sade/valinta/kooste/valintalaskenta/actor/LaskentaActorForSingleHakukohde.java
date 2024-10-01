package fi.vm.sade.valinta.kooste.valintalaskenta.actor;

import static fi.vm.sade.valinta.kooste.valintalaskenta.actor.LaskentaActorForSingleHakukohde.State.*;
import static fi.vm.sade.valintalaskenta.domain.dto.seuranta.IlmoitusDto.ilmoitus;
import static fi.vm.sade.valintalaskenta.domain.dto.seuranta.IlmoitusDto.virheilmoitus;

import fi.vm.sade.valinta.kooste.valintalaskenta.dto.HakukohdeJaOrganisaatio;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.*;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.LaskentaTyyppi;
import fi.vm.sade.valintalaskenta.laskenta.dao.SeurantaDao;

import java.util.Collection;
import java.util.concurrent.CompletableFuture;
import java.util.function.Function;
import java.util.Arrays;
import java.util.Optional;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.IntStream;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

class LaskentaActorForSingleHakukohde implements LaskentaActor {
  private static final Logger LOG = LoggerFactory.getLogger(LaskentaActorForSingleHakukohde.class);

  private final AtomicReference<State> state = new AtomicReference<>(FIRST_ATTEMPTS);
  private final AtomicInteger successTotal = new AtomicInteger(0);
  private final AtomicInteger retryTotal = new AtomicInteger(0);
  private final AtomicInteger failedTotal = new AtomicInteger(0);
  private final LaskentaDto laskenta;
  private final Collection<HakukohdeJaOrganisaatio> hakukohteet;
  private final Function<? super HakukohdeJaOrganisaatio, ? extends CompletableFuture<?>>
      hakukohteenLaskenta;
  private final SeurantaDao seurantaDao;
  private final int splittaus;
  private final ConcurrentLinkedQueue<HakukohdeJaOrganisaatio> hakukohdeQueue;
  private final ConcurrentLinkedQueue<HakukohdeJaOrganisaatio> retryQueue =
      new ConcurrentLinkedQueue<>();
  private final boolean isValintaryhmalaskenta;
  private Optional<IlmoitusDto> valintaryhmalaskennanTulos;

  public LaskentaActorForSingleHakukohde(
      LaskentaDto laskenta,
      Collection<HakukohdeJaOrganisaatio> hakukohteet,
      Function<? super HakukohdeJaOrganisaatio, ? extends CompletableFuture<?>> hakukohteenLaskenta,
      SeurantaDao seurantaDao,
      int splittaus) {
    this.laskenta = laskenta;
    this.hakukohteet = hakukohteet;
    this.hakukohteenLaskenta = hakukohteenLaskenta;
    this.seurantaDao = seurantaDao;
    this.splittaus = splittaus;
    hakukohdeQueue = new ConcurrentLinkedQueue<>(hakukohteet);
    this.isValintaryhmalaskenta = LaskentaTyyppi.VALINTARYHMA.equals(laskenta.getTyyppi());
    this.valintaryhmalaskennanTulos = Optional.empty();
  }

  public void start() {
    LOG.info(
        "(Uuid={}) Laskenta-actor käynnistetty haulle {}, hakukohteita yhteensä {}, splittaus {} ",
        uuid(),
        this.laskenta.getHakuOid(),
        totalKohteet(),
        splittaus);
    final boolean onkoTarveSplitata = hakukohteet.size() > 20;
    IntStream.range(0, onkoTarveSplitata ? splittaus : 1)
        .forEach(
            i -> {
              LOG.info("Käynnistetään laskenta " + i);
              laskeSeuraavaHakukohde();
            });
  }

  private void laskeSeuraavaHakukohde() {
    final Optional<HakukohdeJaOrganisaatio> hkJaOrg;
    final boolean fromRetryQueue;
    if (FIRST_ATTEMPTS.equals(state.get())) {
      hkJaOrg = Optional.ofNullable(hakukohdeQueue.poll());
      fromRetryQueue = false;
    } else if (RERUNS.equals(state.get())) {
      hkJaOrg = Optional.ofNullable(retryQueue.poll());
      fromRetryQueue = true;
    } else {
      throw new IllegalStateException(
          getClass().getSimpleName() + " on ajamassa laskentaa, vaikka tila on " + state.get());
    }

    if (hkJaOrg.isPresent()) {
      try {
        HakukohdeJaOrganisaatio hakukohdeJaOrganisaatio = hkJaOrg.get();
        hakukohteenLaskenta.apply(hakukohdeJaOrganisaatio).orTimeout(3L, TimeUnit.HOURS).thenApply(result -> {
          handleSuccessfulLaskentaResult(fromRetryQueue, hakukohdeJaOrganisaatio.getHakukohdeOid());
          return null;
        }).exceptionally(e -> {
          handleFailedLaskentaResult(fromRetryQueue, hakukohdeJaOrganisaatio, e);
          return null;
        });
      } catch (Exception e) {
        throw new RuntimeException(e);
      }
    } else {
      handleEmptyWorkQueueResult();
    }
  }

  private void handleSuccessfulLaskentaResult(boolean fromRetryQueue, String hakukohdeOid) {
    if (fromRetryQueue) {
      LOG.info(
          "(Uuid={}) Hakukohteen ({}) laskenta onnistui uudelleenyrityksellä. Valmiita kohteita laskennassa yhteensä {}/{}",
          uuid(),
          hakukohdeOid,
          successTotal.incrementAndGet(),
          totalKohteet());
    } else {
      LOG.info(
          "(Uuid={}) Hakukohteen ({}) laskenta onnistui. Valmiita kohteita laskennassa yhteensä {}/{}",
          uuid(),
          hakukohdeOid,
          successTotal.incrementAndGet(),
          totalKohteet());
    }
    if (!isValintaryhmalaskenta) {
      HakukohdeTila tila = HakukohdeTila.VALMIS;
      try {
        seurantaDao.merkkaaTila(uuid(), hakukohdeOid, tila);
        LOG.info(
            "(Uuid={}) Hakukohteen ({}) laskenta on valmis, hakukohteen tila saatiin merkattua seurantaan.",
            uuid(),
            hakukohdeOid);
      } catch (Exception e) {
        LOG.error(
            String.format(
                "(UUID = %s) Hakukohteen (%s) tilan (%s) merkkaaminen epaonnistui!",
                uuid(), hakukohdeOid, tila), e);
      }
    } else {
      LOG.info(
          "Ei merkitä valintaryhmälaskennan hakukohteiden tilaa seurantaan. (Onnistunut laskenta)");
    }
    laskeSeuraavaHakukohde();
  }

  private void handleFailedLaskentaResult(
      boolean fromRetryQueue, HakukohdeJaOrganisaatio hakukohdeJaOrganisaatio, Throwable failure) {
    String hakukohdeOid = hakukohdeJaOrganisaatio.getHakukohdeOid();
    if (!fromRetryQueue) {
      LOG.warn(
          "(Uuid={}) Lisätään hakukohde ({}) epäonnistuneiden jonoon uudelleenyritystä varten. Uudelleenyritettäviä kohteita laskennassa yhteensä {}/{}",
          uuid(),
          hakukohdeOid,
          retryTotal.incrementAndGet(),
          totalKohteet(),
          failure);
      retryQueue.add(hakukohdeJaOrganisaatio);
    } else {
      LOG.error(
          "(Uuid={}) Hakukohteen ({}) laskenta epäonnistui myös uudelleenyrityksellä. Lopullisesti epäonnistuneita kohteita laskennassa yhteensä {}/{}",
          uuid(),
          hakukohdeOid,
          failedTotal.incrementAndGet(),
          totalKohteet(),
          failure);
      if (!isValintaryhmalaskenta) {
        HakukohdeTila tila = HakukohdeTila.KESKEYTETTY;
        try {
          seurantaDao.merkkaaTila(
              uuid(),
              hakukohdeOid,
              tila,
              virheilmoitus(
                  failure.getMessage(), Arrays.toString(failure.getStackTrace())));
          LOG.error(
              "(Uuid={}) Laskenta epäonnistui hakukohteelle {}, tila merkattu onnistuneesti seurantaan ",
              uuid(),
              hakukohdeOid);
        } catch(Exception e) {
          LOG.error(
              String.format(
                  "(UUID = %s) Hakukohteen (%s) tilan (%s) merkkaaminen epaonnistui!",
                  uuid(), hakukohdeOid, tila),
              e);
        }
      } else {
        LOG.error(
            "(Uuid={}) Valintaryhmälaskenta on lopullisesti epäonnistunut: {}.",
            uuid(),
            failure.getMessage());
        this.valintaryhmalaskennanTulos =
            Optional.of(
                virheilmoitus(
                    "Valintaryhmälaskenta epäonnistui: " + failure.getMessage(),
                    Arrays.toString(failure.getStackTrace())));
      }
    }
    laskeSeuraavaHakukohde();
  }

  private void handleEmptyWorkQueueResult() {
    if (state.compareAndSet(FIRST_ATTEMPTS, RERUNS)) {
      if (retryQueue.peek() != null) {
        LOG.info(
            "Laskenta (uuid={}) olisi päättynyt, mutta sisältää keskeytettyjä hakukohteita. Yritetään epäonnistuneita kohteita ({} kpl) uudelleen.",
            uuid(),
            retryTotal.get());
        final boolean splitRetry = retryQueue.size() > 20;
        IntStream.range(0, splitRetry ? splittaus : 1).forEach(i -> laskeSeuraavaHakukohde());
        return;
      } else {
        LOG.info(
            "Laskennassa (uuid={}) ei ole epäonnistuneita hakukohteita uudelleenyritettäviksi.",
            uuid());
      }
    }
    if (totalKohteet() == (successTotal.get() + failedTotal.get())) {
      if (COMPLETE.equals(state.getAndSet(COMPLETE))) {
        LOG.error(
            "state == " + state + " but it is being set to that again! Looks like a bug!",
            new Exception());
      }
      lopeta();
    }
  }

  public void lopeta() {
    if (!COMPLETE.equals(state.get())) {
      LOG.warn("#### (Uuid={}) Laskenta lopetettu", uuid());
      seurantaDao.merkkaaTila(
          uuid(), LaskentaTila.PERUUTETTU, Optional.of(ilmoitus("Laskenta on peruutettu")));
    } else if (valintaryhmalaskennanTulos.isPresent()) {
      LOG.error("#### (Uuid={}) Valintaryhmälaskenta on epäonnistunut.", uuid());
      seurantaDao.merkkaaTila(uuid(), LaskentaTila.PERUUTETTU, valintaryhmalaskennanTulos);
    } else {
      LOG.info(
          "#### (Uuid={}) Laskenta valmis koska ei enää hakukohteita käsiteltävänä. "
              + "Onnistuneita {}, Uudelleenyrityksiä {}, Lopullisesti epäonnistuneita {}",
          uuid(),
          successTotal.get(),
          retryTotal.get(),
          failedTotal.get());
      seurantaDao.merkkaaTila(uuid(), LaskentaTila.VALMIS, Optional.empty());
    }
  }

  private String uuid() {
    return this.laskenta.getUuid();
  }

  private int totalKohteet() {
    return hakukohteet.size();
  }

  public enum State {
    FIRST_ATTEMPTS,
    RERUNS,
    COMPLETE
  }
}
