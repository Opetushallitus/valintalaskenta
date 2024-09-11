package fi.vm.sade.valinta.kooste.proxy.resource.valintatulosservice;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.module.SimpleModule;
import fi.vm.sade.sijoittelu.domain.Valintatulos;
import fi.vm.sade.valinta.kooste.external.resource.valintatulosservice.ValintaTulosServiceAsyncResource;
import java.io.IOException;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import org.apache.commons.lang3.StringUtils;
import org.joda.time.DateTime;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.context.request.async.DeferredResult;

@RestController("ValintaTulosServiceProxyResource")
@RequestMapping("/resources/proxy/valintatulosservice")
public class ValintaTulosServiceProxyResource {
  private static final Logger LOG = LoggerFactory.getLogger(ValintaTulosServiceProxyResource.class);

  @Autowired private ValintaTulosServiceAsyncResource valintaTulosServiceResource;

  @GetMapping(value = "/ilmanhakijantilaa/haku/{hakuOid}/hakukohde/{hakukohdeOid:.+}")
  @PreAuthorize(
      "hasAnyRole('ROLE_APP_SIJOITTELU_READ','ROLE_APP_SIJOITTELU_READ_UPDATE','ROLE_APP_SIJOITTELU_CRUD')")
  public DeferredResult<ResponseEntity<List<Valintatulos>>> valintatuloksetIlmanTilaaHakijalle(
      @PathVariable("hakuOid") String hakuOid,
      @PathVariable("hakukohdeOid") String hakukohdeOid,
      @RequestParam(value = "valintatapajonoOid", required = false) String valintatapajonoOid) {

    DeferredResult<ResponseEntity<List<Valintatulos>>> result =
        new DeferredResult<>(5 * 60 * 1000l);
    result.onTimeout(
        () -> {
          LOG.error(
              String.format(
                  "ValintatulosserviceProxy -palvelukutsu on aikakatkaistu: /ilmanhakijantilaa/haku/%s/hakukohde/%s",
                  hakuOid, hakukohdeOid));
          result.setErrorResult(
              ResponseEntity.status(HttpStatus.REQUEST_TIMEOUT)
                  .body("ValintatulosserviceProxy -palvelukutsu on aikakatkaistu"));
        });

    valintaTulosServiceResource
        .findValintatuloksetIlmanHakijanTilaa(hakuOid, hakukohdeOid)
        .map(
            valintatulokset -> {
              if (StringUtils.isBlank(valintatapajonoOid)) {
                return valintatulokset;
              } else {
                return valintatulokset.stream()
                    .filter(v -> valintatapajonoOid.equals(v.getValintatapajonoOid()))
                    .collect(Collectors.toList());
              }
            })
        .subscribe(
            valintatulokset ->
                result.setResult(ResponseEntity.status(HttpStatus.OK).body(valintatulokset)),
            error -> {
              LOG.error(
                  "Valintatulosten haku ilman valintatuloksen tilaa valinta-tulos-servicestä epäonnistui",
                  error);
              result.setErrorResult(
                  ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                      .body(
                          "ValintatulosserviceProxy -palvelukutsu epäonnistui virheeseen: "
                              + error.getMessage()));
            });

    return result;
  }

  @PostMapping(
      value = "/myohastyneet/haku/{hakuOid}/hakukohde/{hakukohdeOid:.+}",
      produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  @PreAuthorize(
      "hasAnyRole('ROLE_APP_SIJOITTELU_READ','ROLE_APP_SIJOITTELU_READ_UPDATE','ROLE_APP_SIJOITTELU_CRUD')")
  public DeferredResult<ResponseEntity<List<VastaanottoAikarajaMennytDTO>>>
      tietoSiitaEttaVastaanottoAikarajaOnMennyt(
          @PathVariable("hakuOid") String hakuOid,
          @PathVariable("hakukohdeOid") String hakukohdeOid,
          @RequestBody Set<String> hakemusOids) {

    DeferredResult<ResponseEntity<List<VastaanottoAikarajaMennytDTO>>> result =
        new DeferredResult<>(5 * 60 * 1000l);
    result.onTimeout(
        () -> {
          LOG.error(
              String.format(
                  "ValintatulosserviceProxy -palvelukutsu vastaanottoaikarajojen menneeksi on aikakatkaistu: /myohastyneet/haku/%s/hakukohde/%s",
                  hakuOid, hakukohdeOid));
          result.setErrorResult(
              ResponseEntity.status(HttpStatus.REQUEST_TIMEOUT)
                  .body("ValintatulosserviceProxy -palvelukutsu on aikakatkaistu"));
        });

    valintaTulosServiceResource
        .findVastaanottoAikarajaMennyt(hakuOid, hakukohdeOid, hakemusOids)
        .subscribe(
            aikarajaMennytDtos ->
                result.setResult(ResponseEntity.status(HttpStatus.OK).body(aikarajaMennytDtos)),
            error -> {
              LOG.error(
                  "Vastaanottoaikarajojen haku valinta-tulos-servicestä haun "
                      + hakuOid
                      + " hakukohteelle "
                      + hakukohdeOid
                      + " epäonnistui",
                  error);
              result.setErrorResult(
                  ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                      .body(
                          "ValintatulosserviceProxy -palvelukutsu epäonnistui virheeseen: "
                              + error.getMessage()));
            });
    return result;
  }

  @PostMapping(
      value =
          "/tilahakijalle/haku/{hakuOid}/hakukohde/{hakukohdeOid}/valintatapajono/{valintatapajonoOid:.+}",
      produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  @PreAuthorize(
      "hasAnyRole('ROLE_APP_SIJOITTELU_READ','ROLE_APP_SIJOITTELU_READ_UPDATE','ROLE_APP_SIJOITTELU_CRUD')")
  public DeferredResult<ResponseEntity<List<TilaHakijalleDto>>> vastaanottotilaHakijalle(
      @PathVariable("hakuOid") String hakuOid,
      @PathVariable("hakukohdeOid") String hakukohdeOid,
      @PathVariable("valintatapajonoOid") String valintatapajonoOid,
      @RequestBody Set<String> hakemusOids) {

    DeferredResult<ResponseEntity<List<TilaHakijalleDto>>> result =
        new DeferredResult<>(5 * 60 * 1000l);
    result.onTimeout(
        () -> {
          LOG.error(
              String.format(
                  "ValintatulosserviceProxy -palvelukutsu tila hakijalle -tiedon hakemiseksi on aikakatkaistu: /tilahakijalle/haku/%s/hakukohde/%s/valintatapajono/%s",
                  hakuOid, hakukohdeOid, valintatapajonoOid));
          result.setErrorResult(
              ResponseEntity.status(HttpStatus.REQUEST_TIMEOUT)
                  .body("ValintatulosserviceProxy -palvelukutsu on aikakatkaistu"));
        });

    valintaTulosServiceResource
        .findTilahakijalle(hakuOid, hakukohdeOid, valintatapajonoOid, hakemusOids)
        .subscribe(
            tilaHakijalleDtos ->
                result.setResult(ResponseEntity.status(HttpStatus.OK).body(tilaHakijalleDtos)),
            error -> {
              LOG.error(
                  "Hakijan tilojen haku valinta-tulos-servicestä haun "
                      + hakuOid
                      + " hakukohteen "
                      + hakukohdeOid
                      + " valintatapajonolle "
                      + valintatapajonoOid
                      + " epäonnistui",
                  error);
              result.setErrorResult(
                  ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                      .body(
                          "ValintatulosserviceProxy -palvelukutsu epäonnistui virheeseen: "
                              + error.getMessage()));
            });
    return result;
  }

  @GetMapping(
      value =
          "/hakemus/{hakemusOid}/haku/{hakuOid}/hakukohde/{hakukohdeOid}/valintatapajono/{valintatapajonoOid:.+}",
      produces = MediaType.APPLICATION_JSON_VALUE)
  @PreAuthorize(
      "hasAnyRole('ROLE_APP_SIJOITTELU_READ','ROLE_APP_SIJOITTELU_READ_UPDATE','ROLE_APP_SIJOITTELU_CRUD')")
  public DeferredResult<ResponseEntity<List<Valintatulos>>> hakemuksenSijoittelunTulos(
      @PathVariable("hakemusOid") String hakemusOid,
      @PathVariable("hakuOid") String hakuOid,
      @PathVariable("hakukohdeOid") String hakukohdeOid,
      @PathVariable("valintatapajonoOid") String valintatapajonoOid) {

    DeferredResult<ResponseEntity<List<Valintatulos>>> result =
        new DeferredResult<>(5 * 60 * 1000l);
    result.onTimeout(
        () -> {
          LOG.error(
              String.format(
                  "ValintatulosserviceProxy -palvelukutsu on aikakatkaistu: /hakemus/%s/haku/%s/hakukohde/%s/valintatapajono/%s",
                  hakemusOid, hakuOid, hakukohdeOid, valintatapajonoOid));
          result.setErrorResult(
              ResponseEntity.status(HttpStatus.REQUEST_TIMEOUT)
                  .body("ValintatulosserviceProxy -palvelukutsu on aikakatkaistu"));
        });

    valintaTulosServiceResource
        .findValintatuloksetByHakemus(hakuOid, hakemusOid)
        .map(
            valintatulokset -> {
              if (StringUtils.isBlank(valintatapajonoOid)) {
                return valintatulokset;
              } else {
                return valintatulokset.stream()
                    .filter(v -> valintatapajonoOid.equals(v.getValintatapajonoOid()))
                    .collect(Collectors.toList());
              }
            })
        .subscribe(
            valintatulokset ->
                result.setResult(ResponseEntity.status(HttpStatus.OK).body(valintatulokset)),
            error -> {
              LOG.error("Valintatulosten haku valinta-tulos-servicestä epäonnistui", error);
              result.setErrorResult(
                  ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                      .body(
                          "ValintatulosserviceProxy -palvelukutsu epäonnistui virheeseen: "
                              + error.getMessage()));
            });

    return result;
  }

  @GetMapping(
      value = "/hakemus/{hakemusOid}/haku/{hakuOid:.+}",
      produces = MediaType.APPLICATION_JSON_VALUE)
  @PreAuthorize(
      "hasAnyRole('ROLE_APP_SIJOITTELU_READ','ROLE_APP_SIJOITTELU_READ_UPDATE','ROLE_APP_SIJOITTELU_CRUD')")
  public DeferredResult<ResponseEntity<List<Valintatulos>>> kaikkiHakemuksenSijoittelunTulokset(
      @PathVariable("hakemusOid") String hakemusOid, @PathVariable("hakuOid") String hakuOid) {

    DeferredResult<ResponseEntity<List<Valintatulos>>> result =
        new DeferredResult<>(5 * 60 * 1000l);
    result.onTimeout(
        () -> {
          LOG.error(
              String.format(
                  "ValintatulosserviceProxy -palvelukutsu on aikakatkaistu: /hakemus/%s/haku/%s",
                  hakemusOid, hakuOid));
          result.setErrorResult(
              ResponseEntity.status(HttpStatus.REQUEST_TIMEOUT)
                  .body("ValintatulosserviceProxy -palvelukutsu on aikakatkaistu"));
        });

    valintaTulosServiceResource
        .findValintatuloksetByHakemus(hakuOid, hakemusOid)
        .subscribe(
            valintatulokset ->
                result.setResult(ResponseEntity.status(HttpStatus.OK).body(valintatulokset)),
            error -> {
              LOG.error("Valintatulosten haku valinta-tulos-servicestä epäonnistui", error);
              result.setErrorResult(
                  ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                      .body(
                          "ValintatulosserviceProxy -palvelukutsu epäonnistui virheeseen: "
                              + error.getMessage()));
            });

    return result;
  }

  public static class ValintaTulosServiceSerializersModule extends SimpleModule {
    public ValintaTulosServiceSerializersModule() {
      super(ValintaTulosServiceSerializersModule.class.getSimpleName());
      addSerializer(DateTime.class, new DateTimeJsonSerializer());
    }
  }

  private static class DateTimeJsonSerializer extends JsonSerializer<DateTime> {
    @Override
    public void serialize(
        DateTime dateTime, JsonGenerator jsonGenerator, SerializerProvider serializerProvider)
        throws IOException {
      String timestampAsString =
          ValintaTulosServiceAsyncResource.valintaTulosServiceCompatibleFormatter.print(dateTime);
      jsonGenerator.writeString(timestampAsString);
    }
  }
}
