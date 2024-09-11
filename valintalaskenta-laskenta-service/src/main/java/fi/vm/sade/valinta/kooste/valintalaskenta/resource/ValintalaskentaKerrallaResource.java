package fi.vm.sade.valinta.kooste.valintalaskenta.resource;

import static fi.vm.sade.valinta.seuranta.dto.IlmoitusDto.ilmoitus;
import static java.util.Arrays.asList;

import fi.vm.sade.valinta.kooste.AuthorizationUtil;
import fi.vm.sade.valinta.kooste.dto.Vastaus;
import fi.vm.sade.valinta.kooste.external.resource.seuranta.LaskentaSeurantaAsyncResource;
import fi.vm.sade.valinta.kooste.pistesyotto.service.HakukohdeOIDAuthorityCheck;
import fi.vm.sade.valinta.kooste.security.AuthorityCheckService;
import fi.vm.sade.valinta.kooste.valintalaskenta.dto.Laskenta;
import fi.vm.sade.valinta.kooste.valintalaskenta.dto.Maski;
import fi.vm.sade.valinta.kooste.valintalaskenta.route.ValintalaskentaKerrallaRouteValvomo;
import fi.vm.sade.valinta.seuranta.dto.LaskentaDto;
import fi.vm.sade.valinta.seuranta.dto.LaskentaTila;
import fi.vm.sade.valinta.seuranta.dto.LaskentaTyyppi;
import io.reactivex.Observable;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
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
  @Autowired private LaskentaSeurantaAsyncResource seurantaAsyncResource;
  @Autowired private AuthorityCheckService authorityCheckService;

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
      result.onTimeout(
          () -> {
            LOG.error(
                "Laskennan kaynnistys timeuottasi kutsulle /haku/{}/tyyppi/HAKU?valinnanvaihe={}&valintakoelaskenta={}\r\n{}",
                hakuOid,
                valinnanvaihe,
                valintakoelaskenta);
            result.setErrorResult(
                ResponseEntity.status(HttpStatus.REQUEST_TIMEOUT)
                    .body("Ajo laskennalle aikakatkaistu!"));
          });

      final String userOID = AuthorizationUtil.getCurrentUser();
      valintalaskentaKerrallaService.kaynnistaLaskentaHaulle(
          new LaskentaParams(
              userOID,
              haunnimi,
              nimi,
              LaskentaTyyppi.HAKU,
              valintakoelaskenta,
              valinnanvaihe,
              hakuOid,
              Optional.empty(),
              Boolean.TRUE.equals(erillishaku)),
          result);
    } catch (Throwable e) {
      LOG.error("Laskennan kaynnistamisessa tapahtui odottamaton virhe!", e);
      result.setErrorResult(
          ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
              .body("Odottamaton virhe laskennan kaynnistamisessa! " + e.getMessage()));
    }

    return result;
  }

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
      result.onTimeout(
          () -> {
            final String hakukohdeOids = hakukohdeOidsFromMaskiToString(stringMaski);
            LOG.error(
                "Laskennan kaynnistys timeouttasi kutsulle /haku/{}/tyyppi/{}/whitelist/{}?valinnanvaihe={}&valintakoelaskenta={}\r\n{}",
                hakuOid,
                laskentatyyppi,
                whitelist,
                valinnanvaihe,
                valintakoelaskenta,
                hakukohdeOids);
            result.setErrorResult(
                ResponseEntity.status(HttpStatus.REQUEST_TIMEOUT)
                    .body("Uudelleen ajo laskennalle aikakatkaistu!"));
          });

      Maski maski = whitelist ? Maski.whitelist(stringMaski) : Maski.blacklist(stringMaski);
      final String userOID = AuthorizationUtil.getCurrentUser();

      Observable<HakukohdeOIDAuthorityCheck> authorityCheckObservable;
      if (LaskentaTyyppi.VALINTARYHMA.equals(laskentatyyppi)) {
        authorityCheckService.checkAuthorizationForValintaryhma(
            valintaryhmaOid, valintalaskentaAllowedRoles);
        authorityCheckObservable = Observable.empty();
      } else {
        authorityCheckObservable =
            Observable.fromFuture(
                authorityCheckService.getAuthorityCheckForRoles(valintalaskentaAllowedRoles));
      }

      valintalaskentaKerrallaService.kaynnistaLaskentaHaulle(
          new LaskentaParams(
              userOID,
              haunnimi,
              nimi,
              laskentatyyppi,
              valintakoelaskenta,
              valinnanvaihe,
              hakuOid,
              Optional.of(maski),
              Boolean.TRUE.equals(erillishaku)),
          result,
          authorityCheckObservable);

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

  @PostMapping(value = "/uudelleenyrita/{uuid:.+}", produces = MediaType.APPLICATION_JSON_VALUE)
  public DeferredResult<ResponseEntity<Vastaus>> uudelleenajoLaskennalle(
      @PathVariable("uuid") String uuid) {
    DeferredResult<ResponseEntity<Vastaus>> result = new DeferredResult<>(1 * 60 * 1000l);
    result.onTimeout(
        () -> {
          LOG.error("Uudelleen ajo laskennalle({}) timeouttasi!", uuid);
          result.setErrorResult(
              ResponseEntity.status(HttpStatus.REQUEST_TIMEOUT)
                  .body("Uudelleen ajo laskennalle timeouttasi!"));
        });

    checkAuthorizationForLaskentaFromSeuranta(uuid)
        .subscribe(
            allowed -> {
              kaynnistaLaskentaUudelleen(uuid, result);
            },
            error -> {
              LOG.error(
                  "Valintalaskennan uudelleenajo epäonnistui, koska käyttöoikeudet eivät riittäneet!");
              result.setErrorResult(
                  ResponseEntity.status(HttpStatus.FORBIDDEN).body(error.getMessage()));
            });

    return result;
  }

  private void kaynnistaLaskentaUudelleen(
      String uuid, DeferredResult<ResponseEntity<Vastaus>> result) {
    try {
      valintalaskentaKerrallaService.kaynnistaLaskentaUudelleen(uuid, result);
    } catch (Throwable e) {
      LOG.error("Laskennan kaynnistamisessa tapahtui odottamaton virhe", e);
      result.setErrorResult(
          ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
              .body("Odottamaton virhe laskennan kaynnistamisessa! " + e.getMessage()));
    }
  }

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

  @GetMapping(value = "/status/{uuid:.+}", produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(
      summary = "Valintalaskennan tila",
      responses = {
        @ApiResponse(
            responseCode = "OK",
            content = @Content(schema = @Schema(implementation = Laskenta.class)))
      })
  public ResponseEntity<? extends Object> status(@PathVariable("uuid") String uuid) {
    checkAuthorizationForLaskentaFromSeuranta(uuid);
    Optional<ResponseEntity<Laskenta>> result =
        valintalaskentaValvomo
            .fetchLaskenta(uuid)
            .map(l -> ResponseEntity.status(HttpStatus.OK).body(l));

    if (result.isPresent()) {
      return result.get();
    }
    return ResponseEntity.status(HttpStatus.NOT_FOUND).body("Valintalaskenta ei ole muistissa!");
  }

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

    checkAuthorizationForLaskentaFromSeuranta(uuid)
        .subscribe(
            allowed -> {
              valintalaskentaStatusExcelHandler.getStatusXls(uuid, result);
            },
            error -> {
              LOG.error(
                  "Valintalaskennan tilan haku epäonnistui, koska käyttöoikeudet eivät riittäneet!");
              result.setErrorResult(
                  ResponseEntity.status(HttpStatus.FORBIDDEN).body(error.getMessage()));
            });

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

    checkAuthorizationForLaskentaFromSeuranta(uuid)
        .subscribe(
            allowed -> {
              seurantaAsyncResource
                  .laskenta(uuid)
                  .subscribe(
                      laskenta -> result.setResult(ResponseEntity.of(Optional.of(laskenta))),
                      poikkeus -> {
                        LOG.error(
                            "Tietojen haussa seurantapalvelusta(/laskenta/"
                                + uuid
                                + ") tapahtui virhe",
                            poikkeus);
                        result.setErrorResult(poikkeus);
                      });
            },
            error -> {
              LOG.error(
                  "Valintalaskennan tilan haku epäonnistui, koska käyttöoikeudet eivät riittäneet!");
              result.setErrorResult(
                  ResponseEntity.status(HttpStatus.FORBIDDEN).body(error.getMessage()));
            });

    return result;
  }

  @DeleteMapping(value = "/haku/{uuid:.+}")
  public ResponseEntity<String> lopetaLaskenta(
      @PathVariable("uuid") String uuid,
      @RequestParam(value = "lopetaVainJonossaOlevaLaskenta", required = false)
          Boolean lopetaVainJonossaOlevaLaskenta) {
    if (uuid == null) {
      return ResponseEntity.status(HttpStatus.BAD_REQUEST).body("Uuid on pakollinen");
    }
    // Jos käyttöoikeustarkastelu epäonnistuu, tulee poikkeus, tämän suoritus
    // keskeytyy ja poikkeus muuttuu http-virhekoodiksi.
    checkAuthorizationForLaskentaFromSeuranta(uuid).blockingFirst();
    peruutaLaskenta(uuid, lopetaVainJonossaOlevaLaskenta);
    // Palauta OK odottamatta vastausta peruutuspyyntöön
    return ResponseEntity.status(HttpStatus.OK).build();
  }

  private void peruutaLaskenta(String uuid, Boolean lopetaVainJonossaOlevaLaskenta) {
    if (Boolean.TRUE.equals(lopetaVainJonossaOlevaLaskenta)) {
      boolean onkoLaskentaVielaJonossa = valintalaskentaValvomo.fetchLaskenta(uuid) == null;
      if (!onkoLaskentaVielaJonossa) {
        // Laskentaa suoritetaan jo joten ei pysayteta
        return;
      }
    }
    stop(uuid);
    seurantaAsyncResource
        .merkkaaLaskennanTila(
            uuid, LaskentaTila.PERUUTETTU, Optional.of(ilmoitus("Peruutettu käyttäjän toimesta")))
        .subscribe(ok -> stop(uuid), nok -> stop(uuid));
    return;
  }

  private void stop(String uuid) {
    valintalaskentaValvomo.fetchLaskenta(uuid).ifPresent(Laskenta::lopeta);
  }

  private String hakukohdeOidsFromMaskiToString(List<String> maski) {
    if (maski != null && !maski.isEmpty()) {
      try {
        Object[] hakukohdeOidArray = maski.toArray();
        StringBuilder sb = new StringBuilder();
        sb.append(
            Arrays.toString(
                Arrays.copyOfRange(hakukohdeOidArray, 0, Math.min(hakukohdeOidArray.length, 10))));
        if (hakukohdeOidArray.length > 10) {
          sb.append(" ensimmaiset 10 hakukohdetta maskissa jossa on yhteensa hakukohteita ")
              .append(hakukohdeOidArray.length);
        } else {
          sb.append(" maskin hakukohteet");
        }
        return sb.toString();
      } catch (Exception e) {
        LOG.error("hakukohdeOidsFromMaskiToString", e);
        return e.getMessage();
      }
    }
    return null;
  }

  private Boolean checkAuthorizationForLaskentaInContext(
      AuthorityCheckService.Context context, LaskentaDto laskentaDto) {
    if (LaskentaTyyppi.HAKU.equals(laskentaDto.getTyyppi())) {
      authorityCheckService.withContext(
          context,
          () -> {
            authorityCheckService.checkAuthorizationForHaku(
                laskentaDto.getHakuOid(), valintalaskentaAllowedRoles);
          });
    } else {
      final List<String> hakukohdeOids =
          laskentaDto.getHakukohteet().stream()
              .map(hk -> hk.getHakukohdeOid())
              .collect(Collectors.toList());
      authorityCheckService.withContext(
          context,
          () -> {
            authorityCheckService.checkAuthorizationForHakukohteet(
                hakukohdeOids, valintalaskentaAllowedRoles);
          });
    }
    return Boolean.TRUE;
  }

  private Observable<Boolean> checkAuthorizationForLaskentaFromSeuranta(String uuid) {
    // Tallenna tätä pyyntöä suorittavan säikeen konteksti, jotta samaan käyttäjätietoon
    // voidaan viitata tarkastelun suorittavasta säikeestä.
    AuthorityCheckService.Context context = authorityCheckService.getContext();
    return getLaskentaDtoFromSeuranta(uuid)
        .map(laskentaDto -> checkAuthorizationForLaskentaInContext(context, laskentaDto));
  }

  private Observable<LaskentaDto> getLaskentaDtoFromSeuranta(String uuid) {
    return seurantaAsyncResource.laskenta(uuid);
  }
}
