package fi.vm.sade.valintalaskenta.runner.resource;

import static java.util.Arrays.asList;

import fi.vm.sade.service.valintaperusteet.dto.HakukohdeViiteDTO;
import fi.vm.sade.valintalaskenta.audit.AuditSession;
import fi.vm.sade.valintalaskenta.runner.util.AuthorizationUtil;
import fi.vm.sade.valintalaskenta.runner.resource.external.valintaperusteet.ValintaperusteetAsyncResource;
import fi.vm.sade.valintalaskenta.runner.security.AuthorityCheckService;
import fi.vm.sade.valintalaskenta.runner.dto.Laskenta;
import fi.vm.sade.valintalaskenta.runner.dto.Maski;
import fi.vm.sade.valintalaskenta.audit.AuditLogUtil;
import fi.vm.sade.valintalaskenta.runner.service.LuoLaskentaService;
import fi.vm.sade.valinta.sharedutils.ValintaperusteetOperation;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.LaskentaDto;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.LaskentaTyyppi;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.TunnisteDto;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;

import java.util.Collections;
import java.util.List;
import java.util.Optional;

import jakarta.servlet.http.HttpServletRequest;
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
public class ValintalaskentaResource {

  private static final Logger LOG = LoggerFactory.getLogger(ValintalaskentaResource.class);

  private static final List<String> valintalaskentaAllowedRoles =
      asList(
          "ROLE_APP_VALINTOJENTOTEUTTAMINEN_CRUD",
          "ROLE_APP_VALINTOJENTOTEUTTAMINEN_READ_UPDATE",
          "ROLE_APP_VALINTOJENTOTEUTTAMINENKK_CRUD",
          "ROLE_APP_VALINTOJENTOTEUTTAMINENKK_READ_UPDATE");

  @Autowired private LuoLaskentaService luoLaskentaService;
  @Autowired private AuthorityCheckService authorityCheckService;
  @Autowired private ValintaperusteetAsyncResource valintaperusteetAsyncResource;

  private static AuditSession laskentaAuditSession(String userOid, HttpServletRequest request) {

    AuditSession auditSession =
        new AuditSession(userOid, Collections.emptyList(), request.getHeader("user-agent"),
            Optional.ofNullable(request.getHeader("x-forwarded-for")).map(s -> s.split(",")[0]).orElse(request.getRemoteAddr()));
    auditSession.setPersonOid(userOid);
    return auditSession;
  }

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
      @RequestParam(value = "erillishaku", required = false) boolean erillishaku,
      @RequestParam(value = "valinnanvaihe", required = false) Optional<Integer> valinnanvaihe,
      @RequestParam(value = "valintakoelaskenta", required = false) boolean valintakoelaskenta,
      @RequestParam(value = "haunnimi", required = false) String haunnimi,
      @RequestParam(value = "nimi", required = false) String nimi,
      HttpServletRequest request) {

    DeferredResult<ResponseEntity<Vastaus>> result = new DeferredResult<>(1 * 60 * 1000l);
    try {
      authorityCheckService.checkAuthorizationForHaku(hakuOid, valintalaskentaAllowedRoles);

      List<HakukohdeViiteDTO> hakukohdeViitteet = valintaperusteetAsyncResource.haunHakukohteet(hakuOid).get();
      TunnisteDto tunniste = luoLaskentaService.kaynnistaLaskentaHaulle(
          AuthorizationUtil.getCurrentUser(),
          haunnimi,
          nimi,
          LaskentaTyyppi.HAKU,
          valintakoelaskenta,
          valinnanvaihe.flatMap(vaihe -> vaihe==-1 ? Optional.empty() : Optional.of(vaihe)),
          hakuOid,
          Optional.empty(),
          erillishaku,
          hakukohdeViitteet);

      result.setResult(ResponseEntity.status(HttpStatus.OK)
          .body(Vastaus.laskennanSeuraus(tunniste.getUuid(), tunniste.getLuotiinkoUusiLaskenta())));

      AuditLogUtil.auditLogLaskenta(laskentaAuditSession(AuthorizationUtil.getCurrentUser(), request),
          ValintaperusteetOperation.LASKENTATOTEUTUS_LUONTI, tunniste.getUuid(), hakuOid,
          hakukohdeViitteet.stream().map(hk -> hk.getOid()).toList(), Optional.empty());
    } catch(AccessDeniedException e) {
      result.setErrorResult(ResponseEntity.status(HttpStatus.FORBIDDEN).body(e.getMessage()));
    } catch (Throwable e) {
      String message = "Laskennan luonnissa haulle " + hakuOid + " tapahtui virhe";
      LOG.error(message, e);
      result.setErrorResult(ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(message));
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
      @RequestParam(value = "erillishaku", required = false) boolean erillishaku,
      @RequestParam(value = "valinnanvaihe", required = false) Optional<Integer> valinnanvaihe,
      @RequestParam(value = "valintakoelaskenta", required = false) boolean valintakoelaskenta,
      @RequestParam(value = "haunnimi", required = false) String haunnimi,
      @RequestParam(value = "nimi", required = false) String nimi,
      @RequestParam(value = "valintaryhma", required = false) String valintaryhmaOid,
      @PathVariable("tyyppi") LaskentaTyyppi laskentatyyppi,
      @PathVariable("whitelist") boolean whitelist,
      @RequestBody List<String> stringMaski,
      HttpServletRequest request) {
    DeferredResult<ResponseEntity<Vastaus>> result = new DeferredResult<>(1 * 60 * 1000l);

    try {
      List<HakukohdeViiteDTO> hakukohdeViitteet = valintaperusteetAsyncResource.haunHakukohteet(hakuOid).get();
      if (LaskentaTyyppi.VALINTARYHMA.equals(laskentatyyppi)) {
        authorityCheckService.checkAuthorizationForValintaryhma(valintaryhmaOid, valintalaskentaAllowedRoles);
      } else {
        authorityCheckService.checkAuthorizationForHakukohteet(
            hakukohdeViitteet.stream().map(hk -> hk.getOid()).toList(), valintalaskentaAllowedRoles);
      }

      TunnisteDto tunniste = luoLaskentaService.kaynnistaLaskentaHaulle(
          AuthorizationUtil.getCurrentUser(),
          haunnimi,
          nimi,
          laskentatyyppi,
          valintakoelaskenta,
          valinnanvaihe.flatMap(vaihe -> vaihe==-1 ? Optional.empty() : Optional.of(vaihe)),
          hakuOid,
          Optional.of(whitelist ? Maski.whitelist(stringMaski) : Maski.blacklist(stringMaski)),
          erillishaku,
          hakukohdeViitteet);
      result.setResult(ResponseEntity.status(HttpStatus.OK)
          .body(Vastaus.laskennanSeuraus(tunniste.getUuid(), tunniste.getLuotiinkoUusiLaskenta())));

      AuditLogUtil.auditLogLaskenta(laskentaAuditSession(AuthorizationUtil.getCurrentUser(), request),
          ValintaperusteetOperation.LASKENTATOTEUTUS_LUONTI, tunniste.getUuid(), hakuOid,
          hakukohdeViitteet.stream().map(hk -> hk.getOid()).toList(), Optional.empty());
    } catch (AccessDeniedException e) {
      result.setErrorResult(ResponseEntity.status(HttpStatus.FORBIDDEN).body(e.getMessage()));
    } catch (Throwable e) {
      String message = "Osittaisen laskennan luonnissa haulle " + hakuOid + " tapahtui virhe";
      LOG.error(message, e);
      result.setErrorResult(ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(message));
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
      @PathVariable("uuid") String uuid,
      HttpServletRequest request) {
    DeferredResult<ResponseEntity<Vastaus>> result = new DeferredResult<>(1 * 60 * 1000l);

    try {
      Optional<LaskentaDto> laskenta = luoLaskentaService.haeLaskenta(uuid);
      if(laskenta.isEmpty()) {
        result.setErrorResult(ResponseEntity.status(HttpStatus.GONE).body("Laskentaa" + uuid + " ei löytynyt"));
      } else {
        authorityCheckService.checkAuthorizationForLaskenta(laskenta.get(), valintalaskentaAllowedRoles);
        TunnisteDto tunnisteDto = luoLaskentaService.kaynnistaLaskentaUudelleen(uuid);

        AuditLogUtil.auditLogLaskenta(laskentaAuditSession(AuthorizationUtil.getCurrentUser(), request),
            ValintaperusteetOperation.LASKENTATOTEUTUS_UUDELLEENLUONTI, laskenta.get().getUuid(), laskenta.get().getHakuOid(),
            laskenta.get().getHakukohteet().stream().map(hk -> hk.getHakukohdeOid()).toList(), Optional.empty());

        result.setResult(ResponseEntity.status(HttpStatus.OK)
            .body(Vastaus.laskennanSeuraus(tunnisteDto.getUuid(), tunnisteDto.getLuotiinkoUusiLaskenta())));
      }
    } catch (AccessDeniedException e) {
      result.setErrorResult(ResponseEntity.status(HttpStatus.FORBIDDEN).body(e.getMessage()));
    } catch (Throwable e) {
      String message = "Laskennan uudelleenkäynnistämisessä laskennalle " + uuid + " tapahtui virhe";
      LOG.error(message, e);
      result.setErrorResult(ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(message));
    }

    return result;
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
    try {
      Optional<LaskentaDto> laskenta = luoLaskentaService.haeLaskenta(uuid);
      if(laskenta.isEmpty()) {
        return ResponseEntity.status(HttpStatus.GONE).body("Laskentaa" + uuid + " ei löytynyt");
      }

      authorityCheckService.checkAuthorizationForLaskenta(laskenta.get(), valintalaskentaAllowedRoles);
      return luoLaskentaService
          .fetchLaskenta(uuid)
          .map(l -> ResponseEntity.status(HttpStatus.OK).body((Object)l))
          .orElse(ResponseEntity.status(HttpStatus.GONE).body("Valintalaskenta ei ole muistissa!"));
    } catch (AccessDeniedException e) {
      return ResponseEntity.status(HttpStatus.FORBIDDEN).body(e.getMessage());
    } catch (Throwable e) {
      LOG.error("Virhe laskennan " + uuid + " hakemisessa", e);
      return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
    }
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
    result.onTimeout(() -> result.setErrorResult(StatusExcelUtil.createTimeoutErrorXls(uuid)));

    try {
      Optional<LaskentaDto> laskenta = luoLaskentaService.haeLaskenta(uuid);
      if(laskenta.isEmpty()) {
        result.setErrorResult(ResponseEntity.status(HttpStatus.GONE).body("Laskentaa" + uuid + " ei löytynyt"));
      } else {
        authorityCheckService.checkAuthorizationForLaskenta(laskenta.get(), valintalaskentaAllowedRoles);
        result.setResult(StatusExcelUtil.getStatusXls(laskenta.get()));
      }
    } catch (AccessDeniedException e) {
      result.setErrorResult(ResponseEntity.status(HttpStatus.FORBIDDEN).body(e.getMessage()));
    } catch (Throwable e) {
      String message = "Virhe laskennan " + uuid + " hakemisessa";
      LOG.error(message, e);
      result.setErrorResult(ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(message));
    }
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
          Boolean lopetaVainJonossaOlevaLaskenta,
      HttpServletRequest request) {
    try {
      Optional<LaskentaDto> laskenta = luoLaskentaService.haeLaskenta(uuid);
      if(laskenta.isEmpty()) {
        return ResponseEntity.status(HttpStatus.GONE).body("Laskentaa" + uuid + " ei löytynyt");
      }
      authorityCheckService.checkAuthorizationForLaskenta(laskenta.get(), valintalaskentaAllowedRoles);
      luoLaskentaService.peruutaLaskenta(uuid, lopetaVainJonossaOlevaLaskenta);

      AuditLogUtil.auditLogLaskenta(laskentaAuditSession(AuthorizationUtil.getCurrentUser(), request),
          ValintaperusteetOperation.LASKENTATOTEUTUS_PERUUTUS, laskenta.get().getUuid(), laskenta.get().getHakuOid(),
          laskenta.get().getHakukohteet().stream().map(hk -> hk.getHakukohdeOid()).toList(), Optional.empty());

      return ResponseEntity.status(HttpStatus.OK).build();
    } catch (AccessDeniedException e) {
      return ResponseEntity.status(HttpStatus.FORBIDDEN).body(e.getMessage());
    } catch (Throwable e) {
      String message = "Virhe laskennan " + uuid + " lopettamisessa";
      LOG.error(message, e);
      return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(message);
    }
  }
}
