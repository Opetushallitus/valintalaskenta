package fi.vm.sade.valinta.kooste.external.resource.valintapiste;

import static fi.vm.sade.javautils.httpclient.OphHttpClient.Header.ACCEPT;
import static fi.vm.sade.javautils.httpclient.OphHttpClient.Header.CONTENT_TYPE;

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;
import fi.vm.sade.valinta.kooste.external.resource.UrlConfiguration;
import fi.vm.sade.valinta.kooste.external.resource.valintapiste.dto.PisteetWithLastModified;
import fi.vm.sade.valinta.kooste.external.resource.valintapiste.dto.Valintapisteet;
import fi.vm.sade.valinta.kooste.AuditSession;
import fi.vm.sade.valinta.kooste.external.resource.RestCasClient;
import fi.vm.sade.valinta.sharedutils.http.DateDeserializer;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

@Service
public class ValintapisteAsyncResourceImpl implements ValintapisteAsyncResource {
  private final RestCasClient casClient;

  public static final Gson GSON = DateDeserializer.gsonBuilder().create();

  private final UrlConfiguration urlConfiguration;

  public ValintapisteAsyncResourceImpl(
      @Qualifier("ValintapisteServiceCasClient") RestCasClient casClient) {
    this.casClient = casClient;
    this.urlConfiguration = UrlConfiguration.getInstance();
  }

  private void setAuditInfo(Map<String, String> query, AuditSession auditSession) {
    query.put("sessionId", auditSession.getSessionId());
    query.put("uid", auditSession.getPersonOid());
    query.put("inetAddress", auditSession.getInetAddress());
    query.put("userAgent", auditSession.getUserAgent());
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
}
