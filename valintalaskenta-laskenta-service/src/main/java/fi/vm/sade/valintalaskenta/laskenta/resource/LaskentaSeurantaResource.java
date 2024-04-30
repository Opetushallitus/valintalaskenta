package fi.vm.sade.valintalaskenta.laskenta.resource;

import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.OPH_CRUD;

import fi.vm.sade.javautils.nio.cas.CasClient;
import fi.vm.sade.valintalaskenta.laskenta.resource.dto.seuranta.*;
import fi.vm.sade.valintalaskenta.tulos.RestClientUtil;
import java.net.URLEncoder;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutionException;
import org.asynchttpclient.Request;
import org.asynchttpclient.RequestBuilder;
import org.asynchttpclient.Response;
import org.asynchttpclient.util.HttpConstants;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

@RestController
@PreAuthorize("isAuthenticated()")
@RequestMapping(value = "/resources/seuranta")
public class LaskentaSeurantaResource {

  private CasClient seurantaCasClient;

  private String seurantaBaseUrl;

  public LaskentaSeurantaResource(
      @Qualifier("seurantaCasClient") CasClient seurantaCasClient,
      @Value("${valintalaskentakoostepalvelu.seuranta.rest.url}") String seurantaBaseUrl) {
    this.seurantaCasClient = seurantaCasClient;
    this.seurantaBaseUrl = seurantaBaseUrl;
  }

  public static org.asynchttpclient.Response execute(
      final CasClient casClient, final Request request) {
    try {
      return casClient.executeBlocking(request);
    } catch (final ExecutionException e) {
      throw new RuntimeException(e);
    }
  }

  /** Yhteenvedot olemassa olevista laskennoista haulle */
  @PreAuthorize(OPH_CRUD)
  @GetMapping(value = "/hae/{hakuOid}", produces = MediaType.APPLICATION_JSON_VALUE)
  ResponseEntity<String> hae(@PathVariable("hakuOid") String hakuOid) {
    Response response =
        execute(
            this.seurantaCasClient,
            RestClientUtil.request(
                    String.format("%s/seuranta/hae/%s", this.seurantaBaseUrl, hakuOid),
                    HttpConstants.Methods.GET,
                    Collections.emptyMap())
                .build());
    return ResponseEntity.status(response.getStatusCode()).body(response.getResponseBody());
  }

  /** Yhteenvedot olemassa olevista tietyn tyyppisista laskennoista haulle */
  @PreAuthorize(OPH_CRUD)
  @GetMapping(value = "/hae/{hakuOid}/tyyppi/{tyyppi}", produces = MediaType.APPLICATION_JSON_VALUE)
  ResponseEntity<String> hae(
      @PathVariable("hakuOid") String hakuOid, @PathVariable("tyyppi") LaskentaTyyppi tyyppi) {
    Response response =
        execute(
            this.seurantaCasClient,
            RestClientUtil.request(
                    String.format(
                        "%s/seuranta/hae/%s/tyyppi/%s", this.seurantaBaseUrl, hakuOid, tyyppi),
                    HttpConstants.Methods.GET,
                    Collections.emptyMap())
                .build());
    return ResponseEntity.status(response.getStatusCode()).body(response.getResponseBody());
  }

  /** Yhteenvedot olemassa olevista laskennoista haulle */
  @PreAuthorize(OPH_CRUD)
  @GetMapping(value = "/hae/{hakuOid}/kaynnissa", produces = MediaType.APPLICATION_JSON_VALUE)
  ResponseEntity<String> haeKaynnissaOlevatLaskennat(@PathVariable("hakuOid") String hakuOid) {
    Response response =
        execute(
            this.seurantaCasClient,
            RestClientUtil.request(
                    String.format("%s/seuranta/hae/%s/kaynnissa", this.seurantaBaseUrl, hakuOid),
                    HttpConstants.Methods.GET,
                    Collections.emptyMap())
                .build());
    return ResponseEntity.status(response.getStatusCode()).body(response.getResponseBody());
  }

  /** Yhteenvedot olemassa olevista laskennoista */
  @PreAuthorize(OPH_CRUD)
  @GetMapping(
      value = "/yhteenvetokaikillelaskennoille",
      produces = MediaType.APPLICATION_JSON_VALUE)
  ResponseEntity<String> haeYhteenvetoKaikilleLaskennoille() {
    Response response =
        execute(
            this.seurantaCasClient,
            RestClientUtil.request(
                    String.format(
                        "%s/seuranta/yhteenvetokaikillelaskennoille", this.seurantaBaseUrl),
                    HttpConstants.Methods.GET,
                    Collections.emptyMap())
                .build());
    return ResponseEntity.status(response.getStatusCode()).body(response.getResponseBody());
  }

  @PreAuthorize(OPH_CRUD)
  @GetMapping(
      value = "/laskenta/otaSeuraavaLaskentaTyonAlle",
      produces = MediaType.TEXT_PLAIN_VALUE)
  ResponseEntity<String> otaSeuraavaLaskentaTyonAlle() {
    Response response =
        execute(
            this.seurantaCasClient,
            new RequestBuilder()
                .setUrl(
                    String.format(
                        "%s/seuranta/laskenta/otaSeuraavaLaskentaTyonAlle", this.seurantaBaseUrl))
                .setMethod(HttpConstants.Methods.GET)
                .addHeader("Accept", "text/plain")
                .setRequestTimeout(120000)
                .setReadTimeout(120000)
                .build());
    return ResponseEntity.status(response.getStatusCode()).body(response.getResponseBody());
  }

  @PreAuthorize(OPH_CRUD)
  @GetMapping(value = "/laskenta/{uuid}", produces = MediaType.APPLICATION_JSON_VALUE)
  ResponseEntity<String> laskenta(@PathVariable("uuid") String uuid) {
    Response response =
        execute(
            this.seurantaCasClient,
            RestClientUtil.request(
                    String.format("%s/seuranta/laskenta/%s", this.seurantaBaseUrl, uuid),
                    HttpConstants.Methods.GET,
                    Collections.emptyMap())
                .build());
    return ResponseEntity.status(response.getStatusCode()).body(response.getResponseBody());
  }

  @PreAuthorize(OPH_CRUD)
  @GetMapping(value = "/kuormantasaus/laskenta/{uuid}", produces = MediaType.APPLICATION_JSON_VALUE)
  ResponseEntity<String> kuormantasausLaskenta(@PathVariable("uuid") String uuid) {
    Response response =
        execute(
            this.seurantaCasClient,
            RestClientUtil.request(
                    String.format(
                        "%s/seuranta/kuormantasaus/laskenta/%s", this.seurantaBaseUrl, uuid),
                    HttpConstants.Methods.GET,
                    Collections.emptyMap())
                .build());
    return ResponseEntity.status(response.getStatusCode()).body(response.getResponseBody());
  }

  @PreAuthorize(OPH_CRUD)
  @GetMapping(value = "/lataa/{uuid}", produces = MediaType.APPLICATION_JSON_VALUE)
  ResponseEntity<String> lataa(@PathVariable("uuid") String uuid) {
    Response response =
        execute(
            this.seurantaCasClient,
            RestClientUtil.request(
                    String.format("%s/seuranta/lataa/%s", this.seurantaBaseUrl, uuid),
                    HttpConstants.Methods.GET,
                    Collections.emptyMap())
                .build());
    return ResponseEntity.status(response.getStatusCode()).body(response.getResponseBody());
  }

  @PreAuthorize(OPH_CRUD)
  @GetMapping(value = "/yhteenveto/{uuid}", produces = MediaType.APPLICATION_JSON_VALUE)
  ResponseEntity<String> yhteenveto(@PathVariable("uuid") String uuid) {
    Response response =
        execute(
            this.seurantaCasClient,
            RestClientUtil.request(
                    String.format("%s/seuranta/yhteenveto/%s", this.seurantaBaseUrl, uuid),
                    HttpConstants.Methods.GET,
                    Collections.emptyMap())
                .build());
    return ResponseEntity.status(response.getStatusCode()).body(response.getResponseBody());
  }

  /** Paivittaa yksittaisen hakukohteen tilaa laskennassa */
  @PreAuthorize(OPH_CRUD)
  @PutMapping(
      value = "/kuormantasaus/laskenta/{uuid}/hakukohde/{hakukohdeOid}/tila/{tila}",
      produces = MediaType.APPLICATION_JSON_VALUE)
  ResponseEntity<String> merkkaaHakukohteenTila(
      @PathVariable("uuid") String uuid,
      @PathVariable("hakukohdeOid") String hakukohdeOid,
      @PathVariable("tila") HakukohdeTila tila) {
    Response response =
        execute(
            this.seurantaCasClient,
            RestClientUtil.request(
                    String.format(
                        "%s/seuranta/kuormantasaus/laskenta/%s/hakukohde/%s/tila/%s",
                        this.seurantaBaseUrl, uuid, hakukohdeOid, tila),
                    HttpConstants.Methods.PUT,
                    Collections.emptyMap())
                .build());
    return ResponseEntity.status(response.getStatusCode()).body(response.getResponseBody());
  }

  /** Paivittaa yksittaisen hakukohteen tilaa laskennassa ja jattaa ilmoituksen */
  @PreAuthorize(OPH_CRUD)
  @PostMapping(
      value = "/kuormantasaus/laskenta/{uuid}/hakukohde/{hakukohdeOid}/tila/{tila}",
      consumes = MediaType.APPLICATION_JSON_VALUE,
      produces = MediaType.APPLICATION_JSON_VALUE)
  ResponseEntity<String> merkkaaHakukohteenTila(
      @PathVariable("uuid") String uuid,
      @PathVariable("hakukohdeOid") String hakukohdeOid,
      @PathVariable("tila") HakukohdeTila tila,
      @RequestBody IlmoitusDto ilmoitus) {
    Response response =
        execute(
            this.seurantaCasClient,
            RestClientUtil.request(
                    String.format(
                        "%s/seuranta/kuormantasaus/laskenta/%s/hakukohde/%s/tila/%s",
                        this.seurantaBaseUrl, uuid, hakukohdeOid, tila),
                    HttpConstants.Methods.POST,
                    Collections.emptyMap())
                .setBody(RestClientUtil.GSON.toJson(ilmoitus))
                .addHeader("Content-Type", "application/json")
                .build());
    return ResponseEntity.status(response.getStatusCode()).body(response.getResponseBody());
  }

  /** Jattaa ilmoituksen */
  @PreAuthorize(OPH_CRUD)
  @PostMapping(
      value = "/kuormantasaus/laskenta/{uuid}/hakukohde/{hakukohdeOid}",
      consumes = MediaType.APPLICATION_JSON_VALUE,
      produces = MediaType.APPLICATION_JSON_VALUE)
  ResponseEntity<String> lisaaIlmoitusHakukohteelle(
      @PathVariable("uuid") String uuid,
      @PathVariable("hakukohdeOid") String hakukohdeOid,
      @RequestBody IlmoitusDto ilmoitus) {
    Response response =
        execute(
            this.seurantaCasClient,
            RestClientUtil.request(
                    String.format(
                        "%s/seuranta/kuormantasaus/laskenta/%s/hakukohde/%s",
                        this.seurantaBaseUrl, uuid, hakukohdeOid),
                    HttpConstants.Methods.POST,
                    Collections.emptyMap())
                .setBody(RestClientUtil.GSON.toJson(ilmoitus))
                .addHeader("Content-Type", "application/json")
                .build());
    return ResponseEntity.status(response.getStatusCode()).body(response.getResponseBody());
  }

  /** Resetoi hakukohteiden tilat. Poistaa logit. Sailoo valmiit tilat. */
  @PreAuthorize(OPH_CRUD)
  @PutMapping(
      value = "/kuormantasaus/laskenta/{uuid}/resetoi",
      produces = MediaType.APPLICATION_JSON_VALUE)
  ResponseEntity<String> resetoiTilat(@PathVariable("uuid") String uuid) {
    Response response =
        execute(
            this.seurantaCasClient,
            RestClientUtil.request(
                    String.format(
                        "%s/seuranta/kuormantasaus/laskenta/%s/resetoi",
                        this.seurantaBaseUrl, uuid),
                    HttpConstants.Methods.PUT,
                    Collections.emptyMap())
                .build());
    return ResponseEntity.status(response.getStatusCode()).body(response.getResponseBody());
  }

  /** Paivittaa laskennan tilan */
  @PreAuthorize(OPH_CRUD)
  @PutMapping(
      value = "/kuormantasaus/laskenta/{uuid}/tila/{tila}",
      produces = MediaType.APPLICATION_JSON_VALUE)
  ResponseEntity<String> merkkaaLaskennanTila(
      @PathVariable("uuid") String uuid, @PathVariable("tila") LaskentaTila tila) {
    Response response =
        execute(
            this.seurantaCasClient,
            RestClientUtil.request(
                    String.format(
                        "%s/seuranta/kuormantasaus/laskenta/%s/tila/%s",
                        this.seurantaBaseUrl, uuid, tila),
                    HttpConstants.Methods.PUT,
                    Collections.emptyMap())
                .build());
    return ResponseEntity.status(response.getStatusCode()).body(response.getResponseBody());
  }

  @PreAuthorize(OPH_CRUD)
  @PostMapping(
      value = "/kuormantasaus/laskenta/{uuid}/tila/{tila}",
      consumes = MediaType.APPLICATION_JSON_VALUE,
      produces = MediaType.APPLICATION_JSON_VALUE)
  ResponseEntity<String> merkkaaLaskennanTila(
      @PathVariable("uuid") String uuid,
      @PathVariable("tila") LaskentaTila tila,
      @RequestBody IlmoitusDto ilmoitus) {
    Response response =
        execute(
            this.seurantaCasClient,
            RestClientUtil.request(
                    String.format(
                        "%s/seuranta/kuormantasaus/laskenta/%s/tila/%s",
                        this.seurantaBaseUrl, uuid, tila),
                    HttpConstants.Methods.POST,
                    Collections.emptyMap())
                .setBody(RestClientUtil.GSON.toJson(ilmoitus))
                .addHeader("Content-Type", "application/json")
                .build());
    return ResponseEntity.status(response.getStatusCode()).body(response.getResponseBody());
  }

  /** Paivittaa laskennan tilan ja kaikki hakukohteet samalla */
  @PreAuthorize(OPH_CRUD)
  @PutMapping(
      value = "/kuormantasaus/laskenta/{uuid}/tila/{tila}/hakukohde/{hakukohteentila}",
      produces = MediaType.APPLICATION_JSON_VALUE)
  ResponseEntity<String> merkkaaLaskennanTila(
      @PathVariable("uuid") String uuid,
      @PathVariable("tila") LaskentaTila tila,
      @PathVariable("hakukohteentila") HakukohdeTila hakukohteentila) {
    Response response =
        execute(
            this.seurantaCasClient,
            RestClientUtil.request(
                    String.format(
                        "%s/seuranta/kuormantasaus/laskenta/%s/tila/%s/hakukohde/%s",
                        this.seurantaBaseUrl, uuid, tila, hakukohteentila),
                    HttpConstants.Methods.PUT,
                    Collections.emptyMap())
                .build());
    return ResponseEntity.status(response.getStatusCode()).body(response.getResponseBody());
  }

  @PreAuthorize(OPH_CRUD)
  @PostMapping(
      value = "/kuormantasaus/laskenta/{uuid}/tila/{tila}/hakukohde/{hakukohteentila}",
      consumes = MediaType.APPLICATION_JSON_VALUE,
      produces = MediaType.APPLICATION_JSON_VALUE)
  ResponseEntity<String> merkkaaLaskennanTila(
      @PathVariable("uuid") String uuid,
      @PathVariable("tila") LaskentaTila tila,
      @PathVariable("hakukohteentila") HakukohdeTila hakukohteentila,
      @RequestBody IlmoitusDto ilmoitus) {
    Response response =
        execute(
            this.seurantaCasClient,
            RestClientUtil.request(
                    String.format(
                        "%s/seuranta/kuormantasaus/laskenta/%s/tila/%s/hakukohde/%s",
                        this.seurantaBaseUrl, uuid, tila, hakukohteentila),
                    HttpConstants.Methods.POST,
                    Collections.emptyMap())
                .setBody(RestClientUtil.GSON.toJson(ilmoitus))
                .addHeader("Content-Type", "application/json")
                .build());
    return ResponseEntity.status(response.getStatusCode()).body(response.getResponseBody());
  }

  /**
   * Luo uuden laskennan seurantaan
   *
   * @return UUID
   */
  @PreAuthorize(OPH_CRUD)
  @PostMapping(
      value = "/kuormantasaus/laskenta/{hakuOid}/tyyppi/{tyyppi}",
      consumes = MediaType.APPLICATION_JSON_VALUE,
      produces = MediaType.APPLICATION_JSON_VALUE)
  ResponseEntity<String> luoLaskenta(
      @PathVariable("hakuOid") String hakuOid,
      @PathVariable("tyyppi") LaskentaTyyppi tyyppi,
      @RequestParam("userOID") String userOID,
      @RequestParam("haunnimi") String haunnimi,
      @RequestParam(name = "nimi", required = false) String nimi,
      @RequestParam("erillishaku") Boolean erillishaku,
      @RequestParam(name = "valinnanvaihe", required = false) Integer valinnanvaihe,
      @RequestParam(name = "valintakoelaskenta", required = false) Boolean valintakoelaskenta,
      @RequestBody List<HakukohdeDto> hakukohdeOids) {
    Map<String, List<String>> queryParams =
        new HashMap(
            Map.of(
                "userOID",
                List.of(userOID),
                "haunnimi",
                List.of(URLEncoder.encode(haunnimi)),
                "erillishaku",
                List.of(erillishaku.toString())));
    if (nimi != null) {
      queryParams.put("nimi", List.of(URLEncoder.encode(nimi)));
    }
    if (valinnanvaihe != null) {
      queryParams.put("valinnanvaihe", List.of(valinnanvaihe.toString()));
    }
    if (valintakoelaskenta != null) {
      queryParams.put("valintakoelaskenta", List.of(valintakoelaskenta.toString()));
    }
    Response response =
        execute(
            this.seurantaCasClient,
            RestClientUtil.request(
                    String.format(
                        "%s/seuranta/kuormantasaus/laskenta/%s/tyyppi/%s",
                        this.seurantaBaseUrl, hakuOid, tyyppi),
                    HttpConstants.Methods.POST,
                    queryParams)
                .setBody(RestClientUtil.GSON.toJson(hakukohdeOids))
                .addHeader("Content-Type", "application/json")
                .build());
    return ResponseEntity.status(response.getStatusCode()).body(response.getResponseBody());
  }

  /**
   * Poistaa laskennan
   *
   * @return 200 OK jos onnistui
   */
  @PreAuthorize(OPH_CRUD)
  @DeleteMapping(
      value = "/kuormantasaus/laskenta/{uuid}",
      produces = MediaType.APPLICATION_JSON_VALUE)
  ResponseEntity<String> poistaLaskenta(@PathVariable("uuid") String uuid) {
    Response response =
        execute(
            this.seurantaCasClient,
            RestClientUtil.request(
                    String.format(
                        "%s/seuranta/kuormantasaus/laskenta/%s", this.seurantaBaseUrl, uuid),
                    HttpConstants.Methods.DELETE,
                    Collections.emptyMap())
                .build());
    return ResponseEntity.status(response.getStatusCode()).body(response.getResponseBody());
  }
}
