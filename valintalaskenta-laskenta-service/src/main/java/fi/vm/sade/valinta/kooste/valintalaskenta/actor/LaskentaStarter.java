package fi.vm.sade.valinta.kooste.valintalaskenta.actor;

import akka.actor.ActorRef;
import fi.vm.sade.service.valintaperusteet.dto.HakukohdeViiteDTO;
import fi.vm.sade.valinta.kooste.external.resource.ohjausparametrit.OhjausparametritAsyncResource;
import fi.vm.sade.valinta.kooste.external.resource.ohjausparametrit.dto.ParametritDTO;
import fi.vm.sade.valinta.kooste.external.resource.tarjonta.Haku;
import fi.vm.sade.valinta.kooste.external.resource.tarjonta.TarjontaAsyncResource;
import fi.vm.sade.valinta.kooste.external.resource.valintaperusteet.ValintaperusteetAsyncResource;
import fi.vm.sade.valinta.kooste.external.resource.valintatulosservice.dto.AuditSession;
import fi.vm.sade.valinta.kooste.function.SynkronoituLaskuri;
import fi.vm.sade.valinta.kooste.valintalaskenta.dto.HakukohdeJaOrganisaatio;
import fi.vm.sade.valinta.kooste.valintalaskenta.dto.LaskentaStartParams;
import fi.vm.sade.valinta.kooste.valintalaskenta.dto.Maski;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.*;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.LaskentaTyyppi;
import fi.vm.sade.valintalaskenta.laskenta.dao.SeurantaDao;
import io.reactivex.Observable;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.BiConsumer;
import java.util.stream.Collectors;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class LaskentaStarter {
  private static final Logger LOG = LoggerFactory.getLogger(LaskentaStarter.class);

  private final OhjausparametritAsyncResource ohjausparametritAsyncResource;
  private final ValintaperusteetAsyncResource valintaperusteetAsyncResource;
  private final TarjontaAsyncResource tarjontaAsyncResource;
  private final SeurantaDao seurantaDao;

  @Autowired
  public LaskentaStarter(
      OhjausparametritAsyncResource ohjausparametritAsyncResource,
      ValintaperusteetAsyncResource valintaperusteetAsyncResource,
      TarjontaAsyncResource tarjontaAsyncResource,
      SeurantaDao seurantaDao) {
    this.ohjausparametritAsyncResource = ohjausparametritAsyncResource;
    this.valintaperusteetAsyncResource = valintaperusteetAsyncResource;
    this.tarjontaAsyncResource = tarjontaAsyncResource;
    this.seurantaDao = seurantaDao;
  }

  public void fetchLaskentaParams(
      ActorRef laskennanKaynnistajaActor,
      final String uuid,
      final BiConsumer<Haku, LaskentaActorParams> startActor) {
    Observable.fromFuture(CompletableFuture.completedFuture(seurantaDao.haeLaskenta(uuid).get()))
        .subscribe(
            (LaskentaDto laskenta) -> {
              String hakuOid = laskenta.getHakuOid();
              if (StringUtils.isBlank(hakuOid)) {
                LOG.error("Yritettiin hakea hakukohteita ilman hakuOidia!");
                throw new RuntimeException("Yritettiin hakea hakukohteita ilman hakuOidia!");
              }
              valintaperusteetAsyncResource
                  .haunHakukohteet(hakuOid)
                  .subscribe(
                      (List<HakukohdeViiteDTO> hakukohdeViitteet) -> {
                        Collection<HakukohdeJaOrganisaatio> hakukohdeOids =
                            maskHakukohteet(hakuOid, hakukohdeViitteet, laskenta);
                        if (!hakukohdeOids.isEmpty()) {
                          fetchHakuInformation(
                              laskennanKaynnistajaActor,
                              hakuOid,
                              hakukohdeOids,
                              laskenta,
                              startActor);
                        } else {
                          cancelLaskenta(
                              laskennanKaynnistajaActor,
                              "Haulla "
                                  + laskenta.getUuid()
                                  + " ei saatu hakukohteita! Onko valinnat synkronoitu tarjonnan kanssa?",
                              null,
                              uuid);
                        }
                      },
                      (Throwable t) ->
                          cancelLaskenta(
                              laskennanKaynnistajaActor,
                              "Haun kohteiden haku epäonnistui haulle: " + uuid,
                              Optional.empty(),
                              uuid));
            },
            (Throwable t) ->
                cancelLaskenta(
                    laskennanKaynnistajaActor,
                    "Laskennan haku epäonnistui ",
                    Optional.of(t),
                    uuid));
  }

  private static Collection<HakukohdeJaOrganisaatio> maskHakukohteet(
      String hakuOid, List<HakukohdeViiteDTO> hakukohdeViitteet, LaskentaDto laskenta) {
    LOG.info("Tarkastellaan hakukohdeviitteita haulle {}", hakuOid);

    final List<HakukohdeJaOrganisaatio> haunHakukohdeOidit =
        hakukohdeViitteet != null
            ? publishedNonNulltoHakukohdeJaOrganisaatio(hakukohdeViitteet)
            : new ArrayList<>();
    final Maski maski = createMaskiFromLaskenta(laskenta);

    return maski.maskaa(haunHakukohdeOidit);
  }

  private void fetchHakuInformation(
      ActorRef laskennankaynnistajaActor,
      String hakuOid,
      Collection<HakukohdeJaOrganisaatio> haunHakukohdeOidit,
      LaskentaDto laskenta,
      BiConsumer<Haku, LaskentaActorParams> startActor) {
    AtomicReference<Haku> hakuRef = new AtomicReference<>();
    AtomicReference<LaskentaActorParams> parametritRef = new AtomicReference<>();
    SynkronoituLaskuri counter =
        SynkronoituLaskuri.builder()
            .setLaskurinAlkuarvo(2)
            .setSynkronoituToiminto(() -> startActor.accept(hakuRef.get(), parametritRef.get()))
            .build();
    Observable.fromFuture(tarjontaAsyncResource.haeHaku(hakuOid))
        .subscribe(
            haku -> {
              hakuRef.set(haku);
              counter.vahennaLaskuriaJaJosValmisNiinSuoritaToiminto();
            },
            (Throwable t) ->
                cancelLaskenta(
                    laskennankaynnistajaActor,
                    "Tarjontatietojen haku epäonnistui: ",
                    Optional.of(t),
                    laskenta.getUuid()));

    Observable.fromFuture(ohjausparametritAsyncResource.haeHaunOhjausparametrit(hakuOid))
        .subscribe(
            parametrit -> {
              parametritRef.set(
                  laskentaActorParams(hakuOid, laskenta, haunHakukohdeOidit, parametrit));
              counter.vahennaLaskuriaJaJosValmisNiinSuoritaToiminto();
            },
            (Throwable t) ->
                cancelLaskenta(
                    laskennankaynnistajaActor,
                    "Ohjausparametrien luku epäonnistui: ",
                    Optional.of(t),
                    laskenta.getUuid()));
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

  private static LaskentaActorParams laskentaActorParams(
      String hakuOid,
      LaskentaDto laskenta,
      Collection<HakukohdeJaOrganisaatio> haunHakukohdeOidit,
      ParametritDTO parametrit) {
    return new LaskentaActorParams(
        new LaskentaStartParams(
            koosteAuditSession(laskenta),
            laskenta.getUuid(),
            hakuOid,
            laskenta.isErillishaku(),
            true,
            LaskentaTyyppi.VALINTARYHMA.equals(laskenta.getTyyppi()),
            laskenta.getValinnanvaihe(),
            laskenta.getValintakoelaskenta(),
            haunHakukohdeOidit,
            laskenta.getTyyppi()),
        parametrit);
  }

  private static List<HakukohdeJaOrganisaatio> publishedNonNulltoHakukohdeJaOrganisaatio(
      final List<HakukohdeViiteDTO> hakukohdeViitteet) {
    return hakukohdeViitteet.stream()
        .filter(Objects::nonNull)
        .filter(h -> h.getOid() != null)
        .filter(h -> h.getTila().equals("JULKAISTU"))
        .map(h -> new HakukohdeJaOrganisaatio(h.getOid(), h.getTarjoajaOid()))
        .collect(Collectors.toList());
  }

  private void cancelLaskenta(
      ActorRef laskennanKaynnistajaActor, String msg, Optional<Throwable> t, String uuid) {
    if (t.isPresent()) LOG.error(msg, t);
    else LOG.error(msg);
    LaskentaTila tila = LaskentaTila.VALMIS;
    HakukohdeTila hakukohdetila = HakukohdeTila.KESKEYTETTY;
    Optional<IlmoitusDto> ilmoitusDtoOptional =
        t.map(
            poikkeus -> IlmoitusDto.virheilmoitus(msg, Arrays.toString(poikkeus.getStackTrace())));

    seurantaDao.merkkaaTila(uuid, tila, hakukohdetila, ilmoitusDtoOptional);

    // TODO: tämä toimii käsittääkseni oikein vain jos laskenta oli käynnissä
    laskennanKaynnistajaActor.tell(new LaskentaStarterActor.WorkerAvailable(), ActorRef.noSender());
  }

  private static Maski createMaskiFromLaskenta(final LaskentaDto laskenta) {
    final List<String> hakukohdeOids =
        laskenta.getHakukohteet().stream()
            .filter(h -> !HakukohdeTila.VALMIS.equals(h.getTila()))
            .map(h -> new HakukohdeJaOrganisaatio(h.getHakukohdeOid(), h.getOrganisaatioOid()))
            .map(HakukohdeJaOrganisaatio::getHakukohdeOid)
            .collect(Collectors.toList());

    return Maski.whitelist(hakukohdeOids);
  }
}
