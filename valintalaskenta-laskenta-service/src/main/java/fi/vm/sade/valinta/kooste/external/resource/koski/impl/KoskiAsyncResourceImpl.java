package fi.vm.sade.valinta.kooste.external.resource.koski.impl;

import com.google.common.collect.Lists;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.reflect.TypeToken;
import fi.vm.sade.valinta.kooste.external.resource.HttpClient;
import fi.vm.sade.valinta.kooste.external.resource.koski.KoskiAsyncResource;
import fi.vm.sade.valinta.kooste.external.resource.koski.KoskiOppija;
import fi.vm.sade.valinta.kooste.url.UrlConfiguration;
import fi.vm.sade.valinta.kooste.util.CompletableFutureUtil;
import java.lang.reflect.Type;
import java.net.http.HttpRequest;
import java.time.Duration;
import java.util.Base64;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.stream.Collectors;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

@Service
public class KoskiAsyncResourceImpl implements KoskiAsyncResource {
  private static final Logger LOG = LoggerFactory.getLogger(KoskiAsyncResourceImpl.class);
  private static final Type INPUT_TYPE = new TypeToken<List<String>>() {}.getType();
  private static final Type OUTPUT_TYPE = new TypeToken<List<KoskiOppija>>() {}.getType();

  private final HttpClient httpClient;
  private final int maxOppijatPostSize;
  private final Function<HttpRequest.Builder, HttpRequest.Builder>
      addBasicAuthenticationCredentials;
  private final UrlConfiguration urlConfiguration;

  @Autowired
  public KoskiAsyncResourceImpl(
      UrlConfiguration urlConfiguration,
      @Qualifier("KoskiHttpClient") HttpClient httpClient,
      @Value("${valintalaskentakoostepalvelu.koski.username}") String koskiUsername,
      @Value("${valintalaskentakoostepalvelu.koski.password}") String koskiPassword,
      @Value("${valintalaskentakoostepalvelu.koski.max.oppijat.post.size:1000}")
          int maxOppijatPostSize) {
    this.httpClient = httpClient;
    this.maxOppijatPostSize = maxOppijatPostSize;
    this.addBasicAuthenticationCredentials =
        r -> {
          r.header("Authorization", basicAuth(koskiUsername, koskiPassword));
          return r;
        };
    this.urlConfiguration = urlConfiguration;
  }

  private static String basicAuth(String username, String password) {
    return "Basic " + Base64.getEncoder().encodeToString((username + ":" + password).getBytes());
  }

  @Override
  public CompletableFuture<Set<KoskiOppija>> findKoskiOppijat(List<String> oppijanumerot) {
    return batchedPostOppijasFuture(
            oppijanumerot, urlConfiguration.url("koski.oppijanumeroittain.post"))
        .whenComplete(debugLogOpiskeluoikeusVersiot());
  }

  @Override
  public CompletableFuture<JsonElement> findVersionOfOpiskeluoikeus(
      String opiskeluoikeudenOid, int versionumero) {
    return httpClient
        .getResponse(
            urlConfiguration.url(
                "koski.opiskeluoikeuden.versio", opiskeluoikeudenOid, versionumero),
            Duration.ofMinutes(1),
            addBasicAuthenticationCredentials)
        .thenApplyAsync(r -> httpClient.parseJson(r, JsonElement.class));
  }

  private CompletableFuture<Set<KoskiOppija>> batchedPostOppijasFuture(
      List<String> oppijanumerot, String url) {
    if (oppijanumerot.isEmpty()) {
      LOG.info(
          "Batched POST: empty list of oids provided. Returning an empty set without api call.");
      return CompletableFuture.completedFuture(Collections.emptySet());
    }
    List<List<String>> oidBatches = Lists.partition(oppijanumerot, maxOppijatPostSize);
    LOG.info(
        "Batched POST: {} oids partitioned into {} batches",
        oppijanumerot.size(),
        oidBatches.size());

    return CompletableFutureUtil.sequence(
            oidBatches.stream()
                .map(
                    oidBatch -> {
                      LOG.info("Calling POST url {} with {} opiskelijaOids", url, oidBatch.size());
                      return httpClient.<List<String>, List<KoskiOppija>>postJson(
                          url,
                          Duration.ofMinutes(5),
                          oidBatch,
                          INPUT_TYPE,
                          OUTPUT_TYPE,
                          addBasicAuthenticationCredentials);
                    })
                .collect(Collectors.toList()))
        .thenApplyAsync(
            (List<List<KoskiOppija>> r) ->
                r.stream().flatMap(List::stream).collect(Collectors.toSet()));
  }

  private BiConsumer<Set<KoskiOppija>, Throwable> debugLogOpiskeluoikeusVersiot() {
    return (koskiOppijat, throwable) -> {
      if (LOG.isDebugEnabled() && koskiOppijat != null) {
        koskiOppijat.forEach(this::debugLogOpiskeluoikeusVersiot);
      }
    };
  }

  private void debugLogOpiskeluoikeusVersiot(KoskiOppija oppija) {
    oppija
        .getOpiskeluoikeudet()
        .forEach(
            o -> {
              JsonObject opiskeluoikeus = o.getAsJsonObject();
              LOG.debug(
                  String.format(
                      "Oppijan %s opiskeluoikeus %s : aikaleima=%s , versionumero=%s",
                      oppija.getOppijanumero(),
                      opiskeluoikeus.get("oid").getAsString(),
                      opiskeluoikeus.get("aikaleima").getAsString(),
                      opiskeluoikeus.get("versionumero").getAsInt()));
            });
  }
}
