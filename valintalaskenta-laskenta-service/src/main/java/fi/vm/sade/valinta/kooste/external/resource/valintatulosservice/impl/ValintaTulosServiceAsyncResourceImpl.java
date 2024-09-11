package fi.vm.sade.valinta.kooste.external.resource.valintatulosservice.impl;

import com.google.common.reflect.TypeToken;
import com.google.gson.*;
import fi.vm.sade.sijoittelu.domain.Valintatulos;
import fi.vm.sade.sijoittelu.tulos.dto.HakukohdeDTO;
import fi.vm.sade.sijoittelu.tulos.dto.raportointi.HakijaDTO;
import fi.vm.sade.sijoittelu.tulos.dto.raportointi.HakijaPaginationObject;
import fi.vm.sade.valinta.kooste.external.resource.HttpClient;
import fi.vm.sade.valinta.kooste.external.resource.sijoittelu.ValintatulosUpdateStatus;
import fi.vm.sade.valinta.kooste.external.resource.valintatulosservice.ValintaTulosServiceAsyncResource;
import fi.vm.sade.valinta.kooste.external.resource.valintatulosservice.dto.*;
import fi.vm.sade.valinta.kooste.proxy.resource.valintatulosservice.TilaHakijalleDto;
import fi.vm.sade.valinta.kooste.proxy.resource.valintatulosservice.VastaanottoAikarajaMennytDTO;
import fi.vm.sade.valinta.kooste.url.UrlConfiguration;
import fi.vm.sade.valinta.sharedutils.http.DateDeserializer;
import io.reactivex.Observable;
import java.io.IOException;
import java.lang.reflect.Type;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.time.OffsetDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import org.apache.commons.io.IOUtils;
import org.joda.time.DateTime;
import org.joda.time.format.ISODateTimeFormat;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

@Service
public class ValintaTulosServiceAsyncResourceImpl implements ValintaTulosServiceAsyncResource {
  private final HttpClient client;

  private final UrlConfiguration urlConfiguration;

  @Autowired
  public ValintaTulosServiceAsyncResourceImpl(
      @Qualifier("ValintaTulosServiceHttpClient") HttpClient client) {
    this.client = client;
    this.urlConfiguration = UrlConfiguration.getInstance();
  }

  public static Gson getGson() {
    return DateDeserializer.gsonBuilder()
        .registerTypeAdapter(OffsetDateTime.class, new OffsetDateTimeJsonSerializer())
        .registerTypeAdapter(OffsetDateTime.class, new OffsetDateTimeJsonDeserializer())
        .registerTypeAdapter(DateTime.class, new VtsDateTimeJsonDeserializer())
        .registerTypeAdapter(DateTime.class, new VtsDateTimeJsonSerializer())
        .create();
  }

  @Override
  public Observable<String> getHakemuksenValintatulosAsString(String hakuOid, String hakemusOid) {
    return Observable.fromFuture(
        this.client
            .getResponse(
                this.urlConfiguration.url(
                    "valinta-tulos-service.haku.hakuoid.hakemus", hakuOid, hakemusOid),
                Duration.ofMinutes(30l),
                builder -> builder)
            .thenApply(
                response -> {
                  try {
                    return IOUtils.toString(response.body());
                  } catch (IOException e) {
                    throw new RuntimeException(e);
                  }
                }));
  }

  @Override
  public CompletableFuture<List<HakijaDTO>> getKoulutuspaikalliset(
      String hakuOid, String hakukohdeOid) {
    return this.client
        .<HakijaPaginationObject>getJson(
            this.urlConfiguration.url(
                "valinta-tulos-service.haku.hakukohde.hyvaksytyt", hakuOid, hakukohdeOid),
            Duration.ofMinutes(30l),
            new TypeToken<HakijaPaginationObject>() {}.getType())
        .thenApplyAsync(HakijaPaginationObject::getResults);
  }

  @Override
  public CompletableFuture<List<HakijaDTO>> getKoulutuspaikalliset(String hakuOid) {
    return this.client
        .<HakijaPaginationObject>getJson(
            this.urlConfiguration.url("valinta-tulos-service.haku.hyvaksytyt", hakuOid),
            Duration.ofMinutes(30l),
            new TypeToken<HakijaPaginationObject>() {}.getType())
        .thenApplyAsync(HakijaPaginationObject::getResults);
  }

  @Override
  public CompletableFuture<HakijaDTO> getHakijaByHakemus(String hakuOid, String hakemusOid) {
    return this.client.getJson(
        this.urlConfiguration.url(
            "valinta-tulos-service.haku.sijoitteluajo.latest.hakemus", hakuOid, hakemusOid),
        Duration.ofMinutes(30),
        new TypeToken<HakijaDTO>() {}.getType());
  }

  @Override
  public CompletableFuture<List<HakijaDTO>> getKaikkiHakijat(String hakuOid, String hakukohdeOid) {
    return this.client
        .<HakijaPaginationObject>getJson(
            this.urlConfiguration.url(
                "valinta-tulos-service.haku.hakukohde.hakijat", hakuOid, hakukohdeOid),
            Duration.ofMinutes(30),
            new TypeToken<HakijaPaginationObject>() {}.getType())
        .thenApplyAsync(HakijaPaginationObject::getResults);
  }

  @Override
  public CompletableFuture<List<HakijaDTO>> getHakijatIlmanKoulutuspaikkaa(String hakuOid) {
    return this.client
        .<HakijaPaginationObject>getJson(
            this.urlConfiguration.url("valinta-tulos-service.haku.ilmanhyvaksyntaa", hakuOid),
            Duration.ofMinutes(30),
            new TypeToken<HakijaPaginationObject>() {}.getType())
        .thenApplyAsync(HakijaPaginationObject::getResults);
  }

  @Override
  public Observable<List<Valintatulos>> findValintatulokset(String hakuOid, String hakukohdeOid) {
    return Observable.fromFuture(
        this.client.getJson(
            this.urlConfiguration.url(
                "valinta-tulos-service.virkailija.valintatulos.haku.hakukohde",
                hakuOid,
                hakukohdeOid),
            Duration.ofMinutes(30),
            new TypeToken<List<Valintatulos>>() {}.getType()));
  }

  @Override
  public Observable<List<Lukuvuosimaksu>> fetchLukuvuosimaksut(
      String hakukohdeOid, AuditSession session) {
    return Observable.fromFuture(
        this.client.postJson(
            this.urlConfiguration.url(
                "valinta-tulos-service.virkailija.valintatulos.lukuvuosimaksu",
                "read",
                hakukohdeOid),
            Duration.ofMinutes(30l),
            Map.of("auditSession", session),
            new TypeToken<>() {}.getType(),
            new TypeToken<List<Lukuvuosimaksu>>() {}.getType()));
  }

  @Override
  public Observable<String> saveLukuvuosimaksut(
      String hakukohdeOid, AuditSession session, List<LukuvuosimaksuMuutos> muutokset) {
    return Observable.fromFuture(
        this.client
            .postJson(
                this.urlConfiguration.url(
                    "valinta-tulos-service.virkailija.valintatulos.lukuvuosimaksu",
                    "write",
                    hakukohdeOid),
                Duration.ofMinutes(30l),
                Map.of("lukuvuosimaksuMuutokset", muutokset, "auditSession", session),
                new TypeToken<>() {}.getType(),
                new TypeToken<Void>() {}.getType())
            .thenApply(r -> "OK"));
  }

  @Override
  public Observable<List<Valintatulos>> findValintatuloksetIlmanHakijanTilaa(
      String hakuOid, String hakukohdeOid) {
    return Observable.fromFuture(
        this.client.getJson(
            this.urlConfiguration.url(
                "valinta-tulos-service.virkailija.valintatulos.ilmanhakijantilaa.haku.hakukohde",
                hakuOid,
                hakukohdeOid),
            Duration.ofMinutes(30),
            new TypeToken<List<Valintatulos>>() {}.getType()));
  }

  @Override
  public Observable<List<Valintatulos>> findValintatuloksetByHakemus(
      String hakuOid, String hakemusOid) {
    return Observable.fromFuture(
        this.client.getJson(
            this.urlConfiguration.url(
                "valinta-tulos-service.virkailija.valintatulos.haku.hakemus", hakuOid, hakemusOid),
            Duration.ofMinutes(30),
            new TypeToken<List<Valintatulos>>() {}.getType()));
  }

  @Override
  public Observable<List<VastaanottoAikarajaMennytDTO>> findVastaanottoAikarajaMennyt(
      String hakuOid, String hakukohdeOid, Set<String> hakemusOids) {
    return Observable.fromFuture(
        this.client.postJson(
            this.urlConfiguration.url(
                "valinta-tulos-service.virkailija.myohastyneet.haku.hakukohde",
                hakuOid,
                hakukohdeOid),
            Duration.ofMinutes(30l),
            hakemusOids,
            new TypeToken<>() {}.getType(),
            new TypeToken<List<VastaanottoAikarajaMennytDTO>>() {}.getType()));
  }

  @Override
  public Observable<List<TilaHakijalleDto>> findTilahakijalle(
      String hakuOid, String hakukohdeOid, String valintatapajonoOid, Set<String> hakemusOids) {
    return Observable.fromFuture(
        this.client.postJson(
            this.urlConfiguration.url(
                "valinta-tulos-service.virkailija.tilahakijalle.haku.hakukohde.valintatapajono",
                hakuOid,
                hakukohdeOid,
                valintatapajonoOid),
            Duration.ofMinutes(30l),
            hakemusOids,
            new TypeToken<>() {}.getType(),
            new TypeToken<List<TilaHakijalleDto>>() {}.getType()));
  }

  @Override
  public Observable<List<ValintatulosUpdateStatus>> postErillishaunValinnantulokset(
      AuditSession auditSession, String valintatapajonoOid, List<Valinnantulos> valinnantulokset) {
    return Observable.fromFuture(
        this.client.postJson(
            this.urlConfiguration.url(
                "valinta-tulos-service.erillishaku.valinnan-tulos", valintatapajonoOid),
            Duration.ofMinutes(30l),
            new ValinnantulosRequest(auditSession, valinnantulokset),
            new TypeToken<>() {}.getType(),
            new TypeToken<List<ValintatulosUpdateStatus>>() {}.getType(),
            builder ->
                builder.setHeader(
                    "X-If-Unmodified-Since", auditSession.getIfUnmodifiedSince().get())));
  }

  @Override
  public Observable<List<Valinnantulos>> getErillishaunValinnantulokset(
      AuditSession auditSession, String valintatapajonoOid) {
    String url =
        this.urlConfiguration.url(
            "valinta-tulos-service.erillishaku.valinnan-tulos", valintatapajonoOid);
    url += "?sessionId=" + URLEncoder.encode(auditSession.getSessionId(), StandardCharsets.UTF_8);
    url += "&uid=" + URLEncoder.encode(auditSession.getUid(), StandardCharsets.UTF_8);
    url +=
        "&inetAddress=" + URLEncoder.encode(auditSession.getInetAddress(), StandardCharsets.UTF_8);
    url += "&userAgent=" + URLEncoder.encode(auditSession.getUserAgent(), StandardCharsets.UTF_8);
    url += "&hyvaksymiskirjeet=true";

    return Observable.fromFuture(
        this.client.getJson(
            url, Duration.ofMinutes(30), new TypeToken<List<Valinnantulos>>() {}.getType()));
  }

  @Override
  public Observable<HakukohdeDTO> getHakukohdeBySijoitteluajoPlainDTO(
      String hakuOid, String hakukohdeOid) {
    return Observable.fromFuture(
        this.client.getJson(
            this.urlConfiguration.url(
                "valinta-tulos-service.sijoittelu.sijoitteluajo.hakukohde",
                hakuOid,
                "latest",
                hakukohdeOid),
            Duration.ofMinutes(30),
            new TypeToken<HakukohdeDTO>() {}.getType()));
  }

  private static class OffsetDateTimeJsonSerializer implements JsonSerializer<OffsetDateTime> {
    @Override
    public JsonElement serialize(
        OffsetDateTime dateTime, Type type, JsonSerializationContext jsonSerializationContext) {
      return new JsonPrimitive(
          DateTimeFormatter.ISO_OFFSET_DATE_TIME.format(
              dateTime.atZoneSameInstant(ZoneId.of("Europe/Helsinki"))));
    }
  }

  private static class OffsetDateTimeJsonDeserializer implements JsonDeserializer<OffsetDateTime> {
    @Override
    public OffsetDateTime deserialize(
        JsonElement json, Type typeOfT, JsonDeserializationContext context)
        throws JsonParseException {
      return OffsetDateTime.from(DateTimeFormatter.ISO_OFFSET_DATE_TIME.parse(json.getAsString()));
    }
  }

  private static class VtsDateTimeJsonDeserializer implements JsonDeserializer<DateTime> {
    @Override
    public DateTime deserialize(JsonElement json, Type typeOfT, JsonDeserializationContext context)
        throws JsonParseException {
      String dateAsString = json.getAsString();
      try {
        return DateTime.parse(dateAsString, valintaTulosServiceCompatibleFormatter);
      } catch (IllegalArgumentException iae) {
        return DateTime.parse(dateAsString, ISODateTimeFormat.dateTime());
      }
    }
  }

  private static class VtsDateTimeJsonSerializer implements JsonSerializer<DateTime> {
    @Override
    public JsonElement serialize(
        DateTime dateTime, Type type, JsonSerializationContext jsonSerializationContext) {
      return new JsonPrimitive(ISODateTimeFormat.dateTime().print(dateTime));
    }
  }
}
