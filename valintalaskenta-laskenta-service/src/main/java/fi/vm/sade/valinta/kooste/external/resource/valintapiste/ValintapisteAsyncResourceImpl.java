package fi.vm.sade.valinta.kooste.external.resource.valintapiste;

import static fi.vm.sade.javautils.httpclient.OphHttpClient.Header.ACCEPT;
import static fi.vm.sade.javautils.httpclient.OphHttpClient.Header.CONTENT_TYPE;

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;
import fi.vm.sade.valinta.kooste.external.resource.valintapiste.dto.PisteetWithLastModified;
import fi.vm.sade.valinta.kooste.external.resource.valintapiste.dto.Valintapisteet;
import fi.vm.sade.valinta.kooste.external.resource.valintatulosservice.dto.AuditSession;
import fi.vm.sade.valinta.kooste.external.resource.viestintapalvelu.RestCasClient;
import fi.vm.sade.valinta.kooste.url.UrlConfiguration;
import fi.vm.sade.valinta.sharedutils.http.DateDeserializer;
import io.reactivex.Observable;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import javax.ws.rs.core.GenericType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

@Service
public class ValintapisteAsyncResourceImpl implements ValintapisteAsyncResource {
  public static final String OK = "";
  private final RestCasClient casClient;

  public static final Gson GSON = DateDeserializer.gsonBuilder().create();

  private final UrlConfiguration urlConfiguration;

  Logger LOG = LoggerFactory.getLogger(ValintapisteAsyncResource.class);

  public ValintapisteAsyncResourceImpl(
      @Qualifier("ValintapisteServiceCasClient") RestCasClient casClient) {
    this.casClient = casClient;
    this.urlConfiguration = UrlConfiguration.getInstance();
  }

  private Observable<PisteetWithLastModified> handleResponse(
      org.asynchttpclient.Response response) {
    if (response.getStatusCode() != 200) {
      return Observable.error(new RuntimeException("Valintapisteitä ei saatu luettua palvelusta!"));
    } else {
      try {
        List<Valintapisteet> pisteet =
            GSON.fromJson(
                response.getResponseBody(), new GenericType<List<Valintapisteet>>() {}.getType());
        if (pisteet == null) {
          String s = response.getResponseBody();
          LOG.error("Valintapisteet null! Response {}", s);
          String url = response.getUri().toUrl();
          return Observable.error(
              new RuntimeException(String.format("Null response for url %s", url)));
        } else {
          return Observable.just(
              new PisteetWithLastModified(
                  Optional.ofNullable(response.getHeader(LAST_MODIFIED)), pisteet));
        }
      } catch (Exception e) {
        return Observable.error(e);
      }
    }
  }

  private void setAuditInfo(Map<String, String> query, AuditSession auditSession) {
    query.put("sessionId", auditSession.getSessionId());
    query.put("uid", auditSession.getPersonOid());
    query.put("inetAddress", auditSession.getInetAddress());
    query.put("userAgent", auditSession.getUserAgent());
  }

  @Override
  public CompletableFuture<PisteetWithLastModified> getValintapisteet(
      String hakuOID, String hakukohdeOID, AuditSession auditSession) {
    Map<String, String> query = new HashMap<>();
    setAuditInfo(query, auditSession);
    String url =
        this.urlConfiguration.url("valintapiste-service.get.pisteet", hakuOID, hakukohdeOID, query);

    return casClient
        .get(url, Map.of("Accept", "application/json"), 10 * 1000)
        .thenApply(
            response ->
                new PisteetWithLastModified(
                    Optional.ofNullable(response.getHeaders().get(LAST_MODIFIED)),
                    GSON.fromJson(
                        response.getResponseBody(),
                        new TypeToken<List<Valintapisteet>>() {}.getType())));
  }

  @Override
  public Observable<PisteetWithLastModified> getValintapisteet(
      Collection<String> hakemusOIDs, AuditSession auditSession) {
    Map<String, String> query = new HashMap<>();
    setAuditInfo(query, auditSession);
    String url =
        this.urlConfiguration.url("valintapiste-service.get.pisteet.with.hakemusoids", query);
    Observable<org.asynchttpclient.Response> response =
        Observable.fromFuture(
            this.casClient.post(url, hakemusOIDs, Collections.emptyMap(), 30 * 60 * 1000));
    return response.switchMap(this::handleResponse);
  }

  @Override
  public CompletableFuture<PisteetWithLastModified> getValintapisteetWithHakemusOidsAsFuture(
      List<String> hakemusOIDs, AuditSession auditSession) {
    Map<String, String> query = new HashMap<>();
    setAuditInfo(query, auditSession);
    String url =
        this.urlConfiguration.url("valintapiste-service.get.pisteet.with.hakemusoids", query);

    return casClient
        .post(
            url,
            hakemusOIDs,
            Map.of(ACCEPT, "application/json", CONTENT_TYPE, "application/json"),
            60 * 10 * 1000)
        .thenApplyAsync(
            response ->
                new PisteetWithLastModified(
                    Optional.ofNullable(response.getHeaders().get(LAST_MODIFIED)),
                    GSON.fromJson(
                        response.getResponseBody(),
                        new TypeToken<List<Valintapisteet>>() {}.getType())));
  }

  @Override
  public CompletableFuture<Set<String>> putValintapisteet(
      Optional<String> ifUnmodifiedSince, List<Valintapisteet> pisteet, AuditSession auditSession) {
    Map<String, String> query = new HashMap<>();
    query.put("save-partially", "true");
    setAuditInfo(query, auditSession);
    String url = this.urlConfiguration.url("valintapiste-service.put.pisteet", query);

    return casClient
        .put(
            url,
            pisteet,
            ifUnmodifiedSince.isPresent()
                ? Map.of(
                    IF_UNMODIFIED_SINCE,
                    ifUnmodifiedSince.get(),
                    "Content-Type",
                    "application/json")
                : Map.of("Content-Type", "application/json"),
            30 * 60 * 1000)
        .thenApplyAsync(
            response -> {
              return GSON.fromJson(
                  response.getResponseBody(), new TypeToken<Set<String>>() {}.getType());
            });
  }
}
