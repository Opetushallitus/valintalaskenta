package fi.vm.sade.valinta.kooste.valintalaskenta.resource;

import static java.util.Arrays.asList;

import fi.vm.sade.service.valintaperusteet.dto.HakukohdeViiteDTO;
import fi.vm.sade.valinta.kooste.AuthorizationUtil;
import fi.vm.sade.valinta.kooste.dto.Vastaus;
import fi.vm.sade.valinta.kooste.external.resource.valintaperusteet.ValintaperusteetAsyncResource;
import fi.vm.sade.valinta.kooste.security.AuthorityCheckService;
import fi.vm.sade.valinta.kooste.valintalaskenta.dto.Laskenta;
import fi.vm.sade.valinta.kooste.valintalaskenta.dto.Maski;
import fi.vm.sade.valinta.kooste.valintalaskenta.route.ValintalaskentaKerrallaRouteValvomo;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.LaskentaDto;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.LaskentaTyyppi;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.TunnisteDto;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import java.util.List;
import java.util.Optional;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.context.request.async.DeferredResult;

@RestController("ValintalaskentaKerrallaResource")
@RequestMapping("/resources/valintalaskentakerralla")
@PreAuthorize("isAuthenticated()")
@Tag(
    name = "/valintalaskentakerralla",
    description = "Valintalaskenta kaikille valinnanvaiheille kerralla")
public class ValintalaskentaKerrallaResource {

  private static final Logger LOG = LoggerFactory.getLogger(ValintalaskentaKerrallaResource.class);

  private static final List<String> valintalaskentaAllowedRoles =
      asList(
          "ROLE_APP_VALINTOJENTOTEUTTAMINEN_CRUD",
          "ROLE_APP_VALINTOJENTOTEUTTAMINEN_READ_UPDATE",
          "ROLE_APP_VALINTOJENTOTEUTTAMINENKK_CRUD",
          "ROLE_APP_VALINTOJENTOTEUTTAMINENKK_READ_UPDATE");

  @Autowired private ValintalaskentaKerrallaRouteValvomo valintalaskentaValvomo;
  @Autowired private ValintalaskentaKerrallaService valintalaskentaKerrallaService;
  @Autowired private ValintalaskentaStatusExcelHandler valintalaskentaStatusExcelHandler;
  @Autowired private AuthorityCheckService authorityCheckService;
  @Autowired private ValintaperusteetAsyncResource valintaperusteetAsyncResource;

  /**
   * Luo seurantaan suoritettavan valintalaskennan koko haulle.
   *
   * @param hakuOid             haun tunniste
   * @param erillishaku         onko kyseessä erillishaku (haku jonka päätteeksi suoritetaan sijoittelu)
   * @param valinnanvaihe       mikä valinnanvaihe lasketaan (null => lasketaan kaikki)
   * @param valintakoelaskenta  onko kyseessä valintakoelaskenta
   * @param haunnimi            haun nimi
   * @param nimi                laskennan nimi
   *
   * @return                    luodun laskennan tiedot
   */
  @PostMapping(value = "/haku/{hakuOid}/tyyppi/HAKU", produces = MediaType.APPLICATION_JSON_VALUE)
  public DeferredResult<ResponseEntity<Vastaus>> valintalaskentaKokoHaulle(
      @PathVariable("hakuOid") String hakuOid,
      @RequestParam(value = "erillishaku", required = false) Boolean erillishaku,
      @RequestParam(value = "valinnanvaihe", required = false) Integer valinnanvaihe,
      @RequestParam(value = "valintakoelaskenta", required = false) Boolean valintakoelaskenta,
      @RequestParam(value = "haunnimi", required = false) String haunnimi,
      @RequestParam(value = "nimi", required = false) String nimi) {
    authorityCheckService.checkAuthorizationForHaku(hakuOid, valintalaskentaAllowedRoles);

    DeferredResult<ResponseEntity<Vastaus>> result = new DeferredResult<>(1 * 60 * 1000l);
    try {
      final String userOID = AuthorizationUtil.getCurrentUser();
      List<HakukohdeViiteDTO> hakukohdeViitteet = valintaperusteetAsyncResource.haunHakukohteet(hakuOid).blockingFirst();
      TunnisteDto tunniste = valintalaskentaKerrallaService.kaynnistaLaskentaHaulle(
          userOID,
          haunnimi,
          nimi,
          LaskentaTyyppi.HAKU,
          valintakoelaskenta,
          valinnanvaihe,
          hakuOid,
          Optional.empty(),
          Boolean.TRUE.equals(erillishaku),
          hakukohdeViitteet);

      result.setResult(ResponseEntity.status(HttpStatus.OK)
          .body(Vastaus.laskennanSeuraus(tunniste.getUuid(), tunniste.getLuotiinkoUusiLaskenta())));
    } catch (Throwable e) {
      LOG.error("Laskennan luonnissa tapahtui odottamaton virhe!", e);
      result.setErrorResult(
          ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
              .body("Odottamaton virhe laskennan luonnissa! " + e.getMessage()));
    }
    return result;
  }

  /**
   * Luo seurantaan suoritettavan valintalaskennan osalle haun hakukohteista.
   *
   * @param hakuOid             haun tunniste
   * @param erillishaku         onko kyseessä erillishaku (haku jonka päätteeksi suoritetaan sijoittelu)
   * @param valinnanvaihe       mikä valinnanvaihe lasketaan (null => lasketaan kaikki)
   * @param valintakoelaskenta  onko kyseessä valintakoelaskenta
   * @param haunnimi            haun nimi
   * @param nimi                laskennan nimi
   * @param valintaryhmaOid     valintaryhmän tunniste (käytetään toistaiseksi vain autorisoinnissa)
   * @param laskentatyyppi      laskennan tyyppi ()
   * @param whitelist           onko laskettavat hakukohteet kaikista hakukohteista määritelty white- vai blacklistinä
   * @param stringMaski         maski joka sisältää write- tai blacklistin
   *
   * @return                    luodun laskennan tiedot
   */
  @PostMapping(
      value = "/haku/{hakuOid}/tyyppi/{tyyppi}/whitelist/{whitelist:.+}",
      consumes = MediaType.APPLICATION_JSON_VALUE,
      produces = MediaType.APPLICATION_JSON_VALUE)
  public DeferredResult<ResponseEntity<Vastaus>> valintalaskentaHaulle(
      @PathVariable("hakuOid") String hakuOid,
      @RequestParam(value = "erillishaku", required = false) Boolean erillishaku,
      @RequestParam(value = "valinnanvaihe", required = false) Integer valinnanvaihe,
      @RequestParam(value = "valintakoelaskenta", required = false) Boolean valintakoelaskenta,
      @RequestParam(value = "haunnimi", required = false) String haunnimi,
      @RequestParam(value = "nimi", required = false) String nimi,
      @RequestParam(value = "valintaryhma", required = false) String valintaryhmaOid,
      @PathVariable("tyyppi") LaskentaTyyppi laskentatyyppi,
      @PathVariable("whitelist") boolean whitelist,
      @RequestBody List<String> stringMaski) {
    DeferredResult<ResponseEntity<Vastaus>> result = new DeferredResult<>(1 * 60 * 1000l);

    try {
      Maski maski = whitelist ? Maski.whitelist(stringMaski) : Maski.blacklist(stringMaski);
      final String userOID = AuthorizationUtil.getCurrentUser();

      List<HakukohdeViiteDTO> hakukohdeViitteet = valintaperusteetAsyncResource.haunHakukohteet(hakuOid).blockingFirst();
      if (LaskentaTyyppi.VALINTARYHMA.equals(laskentatyyppi)) {
        // TODO: kiellä maskin käyttö valintaryhmälaskennassa
        authorityCheckService.checkAuthorizationForValintaryhma(valintaryhmaOid, valintalaskentaAllowedRoles);
      } else {
        authorityCheckService.checkAuthorizationForHakukohteet(hakukohdeViitteet.stream().map(hk -> hk.getOid()).toList(), valintalaskentaAllowedRoles);
      }

      TunnisteDto tunniste = valintalaskentaKerrallaService.kaynnistaLaskentaHaulle(
          userOID,
          haunnimi,
          nimi,
          laskentatyyppi,
          valintakoelaskenta,
          valinnanvaihe,
          hakuOid,
          Optional.of(maski),
          Boolean.TRUE.equals(erillishaku),
          hakukohdeViitteet);
      result.setResult(ResponseEntity.status(HttpStatus.OK)
          .body(Vastaus.laskennanSeuraus(tunniste.getUuid(), tunniste.getLuotiinkoUusiLaskenta())));
    } catch (AccessDeniedException e) {
      result.setErrorResult(ResponseEntity.status(HttpStatus.FORBIDDEN).body(e.getMessage()));
    } catch (Throwable e) {
      LOG.error("Laskennan kaynnistamisessa tapahtui odottamaton virhe!", e);
      result.setErrorResult(
          ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
              .body("Odottamaton virhe laskennan kaynnistamisessa! " + e.getMessage()));
      throw e;
    }

    return result;
  }

  /**
   * Käynnistää olemassaolevan laskennan uudelleen
   *
   * @param uuid  laskennan tunniste
   *
   * @return      uudelleenkäynnistety laskennan tiedot
   */
  @PostMapping(value = "/uudelleenyrita/{uuid:.+}", produces = MediaType.APPLICATION_JSON_VALUE)
  public DeferredResult<ResponseEntity<Vastaus>> uudelleenajoLaskennalle(
      @PathVariable("uuid") String uuid) {
    DeferredResult<ResponseEntity<Vastaus>> result = new DeferredResult<>(1 * 60 * 1000l);

    if(!checkAuthorizationForLaskenta(uuid)) {
      String message = "Valintalaskennan uudelleenajo epäonnistui, koska käyttöoikeudet eivät riittäneet!";
      LOG.error(message);
      result.setErrorResult(ResponseEntity.status(HttpStatus.FORBIDDEN).body(message));
      return result;
    }

    try {
      valintalaskentaKerrallaService.kaynnistaLaskentaUudelleen(uuid);
    } catch (Throwable e) {
      LOG.error("Laskennan kaynnistamisessa tapahtui odottamaton virhe", e);
      result.setErrorResult(
          ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
              .body("Odottamaton virhe laskennan kaynnistamisessa! " + e.getMessage()));
    }

    return result;
  }

  /**
   * Palauttaa käynnissäolevien laskentojen tilan
   *
   * @return
   */
  @GetMapping(value = "/status", produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(
      summary = "Valintalaskennan tila",
      responses = {
        @ApiResponse(
            responseCode = "OK",
            content = @Content(schema = @Schema(implementation = Laskenta.class)))
      })
  public List<Laskenta> status() {
    return valintalaskentaValvomo.runningLaskentas();
  }

  /**
   * Palauttaa yksittäisen laskennan tilan
   *
   * @param uuid  laskennan tunniste
   *
   * @return      laskennan tila
   */
  @GetMapping(value = "/status/{uuid:.+}", produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(
      summary = "Valintalaskennan tila",
      responses = {
        @ApiResponse(
            responseCode = "OK",
            content = @Content(schema = @Schema(implementation = Laskenta.class)))
      })
  public ResponseEntity<? extends Object> status(@PathVariable("uuid") String uuid) {
    if(!checkAuthorizationForLaskenta(uuid)) {
      String message = "Käyttäjällä ei ole oikeuksian laskentaan";
      LOG.error(message);
      return ResponseEntity.status(HttpStatus.FORBIDDEN).body(message);
    }

    Optional<ResponseEntity<Laskenta>> result =
        valintalaskentaValvomo
            .fetchLaskenta(uuid)
            .map(l -> ResponseEntity.status(HttpStatus.OK).body(l));

    if (result.isPresent()) {
      return result.get();
    }
    return ResponseEntity.status(HttpStatus.NOT_FOUND).body("Valintalaskenta ei ole muistissa!");
  }

  /**
   * Palauttaa yksittäisen valintalaskennan tilan Excel-tiedostona
   *
   * @param uuid  valintalaskennan tunniste
   * @return      statusexcel
   */
  @GetMapping(value = "/status/{uuid}/xls", produces = "application/vnd.ms-excel")
  @Operation(
      summary = "Valintalaskennan tila",
      responses = {
        @ApiResponse(
            responseCode = "OK",
            content = @Content(schema = @Schema(implementation = byte[].class)))
      })
  public DeferredResult<ResponseEntity<byte[]>> statusXls(@PathVariable("uuid") final String uuid) {
    DeferredResult<ResponseEntity<byte[]>> result = new DeferredResult<>(15 * 60 * 1000l);
    result.onTimeout(
        () -> {
          result.setErrorResult(valintalaskentaStatusExcelHandler.createTimeoutErrorXls(uuid));
        });

    if(!checkAuthorizationForLaskenta(uuid)) {
      String message = "Valintalaskennan tilan haku epäonnistui, koska käyttöoikeudet eivät riittäneet!";
      LOG.error(message);
      result.setErrorResult(ResponseEntity.status(HttpStatus.FORBIDDEN).body(message));
      return result;
    }

    valintalaskentaStatusExcelHandler.getStatusXls(uuid, result);
    return result;
  }

  @GetMapping(value = "/status/{uuid}/yhteenveto", produces = "application/json")
  @Operation(
      summary = "Valintalaskennan tilan yhteenveto",
      responses = {
        @ApiResponse(
            responseCode = "OK",
            content = @Content(schema = @Schema(implementation = LaskentaDto.class)))
      })
  public DeferredResult<ResponseEntity<LaskentaDto>> statusYhteenveto(
      @PathVariable("uuid") final String uuid) {

    DeferredResult<ResponseEntity<LaskentaDto>> result = new DeferredResult<>(60 * 1000L);
    result.onTimeout(
        () -> {
          LOG.error("Valintalaskennan tilan {} hakeminen timeouttasi!", uuid);
          result.setErrorResult(
              ResponseEntity.status(HttpStatus.REQUEST_TIMEOUT)
                  .body("Valintalaskennan tilan hakeminen timeouttasi!"));
        });

    if(!checkAuthorizationForLaskenta(uuid)) {
      String message = "Valintalaskennan tilan haku epäonnistui, koska käyttöoikeudet eivät riittäneet!";
      LOG.error(message);
      result.setErrorResult(ResponseEntity.status(HttpStatus.FORBIDDEN).body(message));
      return result;
    }

    result.setResult(ResponseEntity.of(valintalaskentaKerrallaService.haeLaskenta(uuid)));
    return result;
  }

  /**
   * Peruuttaa luodun laskennan
   *
   * @param uuid                            laskennan tunniste
   * @param lopetaVainJonossaOlevaLaskenta  peruutetaan laskenta vain jos sitä ei vielä ole käynnistetty
   *
   * @return                                tyhjä
   */
  @DeleteMapping(value = "/haku/{uuid:.+}")
  public ResponseEntity<String> lopetaLaskenta(
      @PathVariable("uuid") String uuid,
      @RequestParam(value = "lopetaVainJonossaOlevaLaskenta", required = false)
          Boolean lopetaVainJonossaOlevaLaskenta) {
    if (uuid == null) {
      return ResponseEntity.status(HttpStatus.BAD_REQUEST).body("Uuid on pakollinen");
    }

    if(!checkAuthorizationForLaskenta(uuid)) {
      String message = "Valintalaskennan lopettaminen epäonnistui, koska käyttöoikeudet eivät riittäneet!";
      LOG.error(message);
      return ResponseEntity.status(HttpStatus.FORBIDDEN).body(message);
    }

    valintalaskentaKerrallaService.peruutaLaskenta(uuid, lopetaVainJonossaOlevaLaskenta);
    // Palauta OK odottamatta vastausta peruutuspyyntöön
    return ResponseEntity.status(HttpStatus.OK).build();
  }

  private Boolean checkAuthorizationForLaskenta(String uuid) {
    return valintalaskentaKerrallaService.haeLaskenta(uuid)
        .map(laskentaDto -> authorityCheckService.checkAuthorizationForLaskenta(
            laskentaDto, valintalaskentaAllowedRoles))
        .orElse(false);
  }
}
