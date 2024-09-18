package fi.vm.sade.valinta.kooste.valintalaskenta.resource;

import fi.vm.sade.service.valintaperusteet.dto.HakukohdeViiteDTO;
import fi.vm.sade.valinta.kooste.dto.Vastaus;
import fi.vm.sade.valinta.kooste.seuranta.LaskentaSeurantaService;
import fi.vm.sade.valinta.kooste.external.resource.valintaperusteet.ValintaperusteetAsyncResource;
import fi.vm.sade.valinta.kooste.security.HakukohdeOIDAuthorityCheck;
import fi.vm.sade.valinta.kooste.valintalaskenta.actor.dto.HakukohdeJaOrganisaatio;
import fi.vm.sade.valinta.kooste.valintalaskenta.dto.Laskenta;
import fi.vm.sade.valinta.kooste.valintalaskenta.dto.LaskentaInfo;
import fi.vm.sade.valinta.kooste.valintalaskenta.dto.Maski;
import fi.vm.sade.valinta.kooste.valintalaskenta.route.ValintalaskentaKerrallaRoute;
import fi.vm.sade.valinta.kooste.valintalaskenta.route.ValintalaskentaKerrallaRouteValvomo;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.HakukohdeDto;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.LaskentaDto;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.LaskentaTyyppi;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.TunnisteDto;
import io.reactivex.Observable;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;
import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.stereotype.Service;
import org.springframework.web.context.request.async.DeferredResult;

@Service
public class ValintalaskentaKerrallaService {
  private static final Logger LOG = LoggerFactory.getLogger(ValintalaskentaKerrallaService.class);

  @Autowired private ValintalaskentaKerrallaRouteValvomo valintalaskentaValvomo;
  @Autowired private ValintaperusteetAsyncResource valintaperusteetAsyncResource;
  @Autowired private ValintalaskentaKerrallaRoute valintalaskentaRoute;
  @Autowired private LaskentaSeurantaService laskentaSeurantaService;

  public ValintalaskentaKerrallaService() {}

  private void autorisoiHakukohteet(String hakuOid,
                                    Collection<HakukohdeJaOrganisaatio> haunHakukohteetOids,
                                    Observable<HakukohdeOIDAuthorityCheck> authCheck) {
    authCheck
        .blockingNext()
        .forEach(
            authorityCheck ->
                haunHakukohteetOids.forEach(
                    hk -> {
                      if (!authorityCheck.test(hk.getHakukohdeOid())) {
                        LOG.error(
                            String.format(
                                "Ei oikeutta aloittaa laskentaa hakukohteelle %s haussa %s",
                                hk.getHakukohdeOid(), hakuOid));
                        throw new AccessDeniedException(
                            "Ei oikeutta aloittaa laskentaa");
                      }
                    }));
  }

  public TunnisteDto kaynnistaLaskentaHaulle(LaskentaParams laskentaParams) {
    return kaynnistaLaskentaHaulle(laskentaParams, Observable.empty());
  }

  public TunnisteDto kaynnistaLaskentaHaulle(
      LaskentaParams laskentaParams,
      Observable<HakukohdeOIDAuthorityCheck> authCheck) {
    String hakuOid = laskentaParams.getHakuOid();
    Optional<String> uuidForExistingNonMaskedLaskenta =
        uuidForExistingNonMaskedLaskenta(laskentaParams.getMaski(), hakuOid);

    if (uuidForExistingNonMaskedLaskenta.isPresent()) {
      String uuid = uuidForExistingNonMaskedLaskenta.get();
      LOG.warn(
          "Laskenta on jo kaynnissa haulle {} joten palautetaan seurantatunnus({}) ajossa olevaan hakuun",
          uuid,
          uuid);
      return new TunnisteDto(uuid, false);
    }

    LOG.info("Aloitetaan laskenta haulle {}", hakuOid);
    List<HakukohdeViiteDTO> hakukohdeViitteet = valintaperusteetAsyncResource.haunHakukohteet(hakuOid).blockingFirst();
    Collection<HakukohdeJaOrganisaatio> haunHakukohteetOids =
        kasitteleHakukohdeViitteet(
            hakukohdeViitteet, hakuOid, laskentaParams.getMaski());

    if (!LaskentaTyyppi.VALINTARYHMA.equals(laskentaParams.getLaskentatyyppi())) {
      this.autorisoiHakukohteet(hakuOid, haunHakukohteetOids, authCheck);
    }

    final List<HakukohdeDto> hakukohdeDtos = toHakukohdeDto(haunHakukohteetOids);
    validateHakukohdeDtos(haunHakukohteetOids, hakukohdeDtos);
    TunnisteDto tunniste =  laskentaSeurantaService.luoLaskenta(laskentaParams, hakukohdeDtos);
    if (tunniste.getLuotiinkoUusiLaskenta()) {
      valintalaskentaRoute.workAvailable();
    }
    return tunniste;
  }

  public void kaynnistaLaskentaUudelleen(
      final String uuid, final DeferredResult<ResponseEntity<Vastaus>> result) {
    valintalaskentaValvomo
        .fetchLaskenta(uuid)
        .filter(laskenta -> !laskenta.isValmis())
        .ifPresentOrElse(
            laskenta -> {
              palautaAjossaolevaLaskenta(uuid, result);
            },
            () -> {
              resetoiTilat(uuid, result);
            });
  }

  private void palautaAjossaolevaLaskenta(
      String uuid, DeferredResult<ResponseEntity<Vastaus>> result) {
    LOG.warn("Laskenta {} on viela ajossa, joten palautetaan linkki siihen.", uuid);
    result.setResult(redirectResponse(new TunnisteDto(uuid, false)));
  }

  private void resetoiTilat(String uuid, DeferredResult<ResponseEntity<Vastaus>> result) {
    laskentaSeurantaService
        .resetoiTilat(uuid)
        .flatMap(
            (LaskentaDto laskenta) ->
                Observable.just(laskenta)
                    .zipWith(
                        valintaperusteetAsyncResource.haunHakukohteet(laskenta.getHakuOid()),
                        Pair::of))
        .subscribe(
            (Pair<LaskentaDto, List<HakukohdeViiteDTO>> laskentaJaHakukohdeViitteet) -> {
              LaskentaDto laskenta = laskentaJaHakukohdeViitteet.getLeft();
              notifyWorkAvailable(
                  new TunnisteDto(laskenta.getUuid(), laskenta.getLuotiinkoUusiLaskenta()), result);
            },
            (Throwable t) -> {
              LOG.error("Laskennan uudelleenajo epäonnistui. Uuid: " + uuid, t);
              result.setErrorResult(errorResponse("Uudelleen ajo laskennalle heitti poikkeuksen!"));
            });
  }

  private Optional<Laskenta> haeAjossaOlevaLaskentaHaulle(final String hakuOid) {
    return valintalaskentaValvomo.runningLaskentas().stream()
        .filter(l -> hakuOid.equals(l.getHakuOid()) && !l.isOsittainenLaskenta())
        .findFirst();
  }

  private static Collection<HakukohdeJaOrganisaatio> kasitteleHakukohdeViitteet(
      final List<HakukohdeViiteDTO> hakukohdeViitteet,
      final String hakuOid,
      final Optional<Maski> maski) {
    LOG.info("Tarkastellaan hakukohdeviitteita haulle {}", hakuOid);

    if (hakukohdeViitteet == null || hakukohdeViitteet.isEmpty()) {
      LOG.error("Valintaperusteet palautti tyhjat hakukohdeviitteet haulle {}!", hakuOid);
      throw new NullPointerException("Valintaperusteet palautti tyhjat hakukohdeviitteet!");
    }
    final List<HakukohdeJaOrganisaatio> haunHakukohdeOids =
        hakukohdeViitteet.stream()
            .filter(Objects::nonNull)
            .filter(hakukohdeOid -> hakukohdeOid.getOid() != null)
            .filter(hakukohdeOid -> hakukohdeOid.getTila().equals("JULKAISTU"))
            .map(u -> new HakukohdeJaOrganisaatio(u.getOid(), u.getTarjoajaOid()))
            .collect(Collectors.toList());

    Collection<HakukohdeJaOrganisaatio> oids =
        maski.map(m -> m.maskaa(haunHakukohdeOids)).orElse(haunHakukohdeOids);
    if (oids.isEmpty()) {
      String msg =
          "Haulla "
              + hakuOid
              + " ei saatu hakukohteita! Onko valinnat synkronoitu tarjonnan kanssa?";
      LOG.error(msg);
      throw new RuntimeException(msg);
    } else {
      return oids;
    }
  }

  private void notifyWorkAvailable(
      final TunnisteDto uuid, final DeferredResult<ResponseEntity<Vastaus>> result) {
    // ohitetaan ajossa olevan laskennan kaynnistaminen
    if (uuid.getLuotiinkoUusiLaskenta()) {
      valintalaskentaRoute.workAvailable();
    }
    result.setResult(redirectResponse(uuid));
  }

  private static void validateHakukohdeDtos(
      Collection<HakukohdeJaOrganisaatio> hakukohdeData,
      List<HakukohdeDto> hakukohdeDtos) {
    if (hakukohdeDtos.isEmpty()) {
      String msg = "Laskentaa ei voida aloittaa hakukohteille joilta puuttuu organisaatio!";
      LOG.error(msg);
      throw new RuntimeException(msg);
    }
    if (hakukohdeDtos.size() < hakukohdeData.size()) {
      LOG.warn(
          "Hakukohteita puuttuvien organisaatio-oidien vuoksi filtteroinnin jalkeen {}/{}!",
          hakukohdeDtos.size(),
          hakukohdeData.size());
    } else {
      LOG.info(
          "Hakukohteita filtteroinnin jalkeen {}/{}!", hakukohdeDtos.size(), hakukohdeData.size());
    }
  }

  private static ResponseEntity redirectResponse(final TunnisteDto target) {
    return ResponseEntity.status(HttpStatus.OK)
        .body(Vastaus.laskennanSeuraus(target.getUuid(), target.getLuotiinkoUusiLaskenta()));
  }

  private static ResponseEntity errorResponse(final String errorMessage) {
    return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(errorMessage);
  }

  private static List<HakukohdeDto> toHakukohdeDto(
      Collection<HakukohdeJaOrganisaatio> hakukohdeData) {
    return hakukohdeData.stream()
        .filter(Objects::nonNull)
        .filter(hk -> hk.getHakukohdeOid() != null)
        .filter(hk -> hk.getOrganisaatioOid() != null)
        .map(hk -> new HakukohdeDto(hk.getHakukohdeOid(), hk.getOrganisaatioOid()))
        .collect(Collectors.toList());
  }

  private Optional<String> uuidForExistingNonMaskedLaskenta(Optional<Maski> maski, String hakuOid) {
    final Optional<Laskenta> ajossaOlevaLaskentaHaulle =
        !maski.isPresent() || !maski.get().isMask()
            ? haeAjossaOlevaLaskentaHaulle(hakuOid)
            : Optional.empty();
    return ajossaOlevaLaskentaHaulle.map(LaskentaInfo::getUuid);
  }
}
