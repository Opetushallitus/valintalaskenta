package fi.vm.sade.valinta.kooste.external.resource.koodisto.impl;

import com.google.gson.reflect.TypeToken;
import fi.vm.sade.valinta.kooste.external.resource.HttpClient;
import fi.vm.sade.valinta.kooste.external.resource.koodisto.KoodistoAsyncResource;
import fi.vm.sade.valinta.kooste.external.resource.koodisto.dto.Koodi;
import fi.vm.sade.valinta.kooste.url.UrlConfiguration;
import java.time.Duration;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

@Service
public class KoodistoAsyncResourceImpl implements KoodistoAsyncResource {
  private final Logger LOG = LoggerFactory.getLogger(getClass());
  private final HttpClient client;
  private final UrlConfiguration urlConfiguration;

  @Autowired
  public KoodistoAsyncResourceImpl(@Qualifier("KoodistoHttpClient") HttpClient client) {
    this.client = client;
    this.urlConfiguration = UrlConfiguration.getInstance();
  }

  @Override
  public CompletableFuture<List<Koodi>> haeKoodisto(String koodistoUri) {
    HashMap<String, Boolean> query = new HashMap<>();
    query.put("onlyValidKoodis", true);
    return this.client.getJson(
        this.urlConfiguration.url("koodisto-service.json.oid.koodi", koodistoUri, query),
        Duration.ofMinutes(1),
        new TypeToken<List<Koodi>>() {}.getType());
  }

  @Override
  public CompletableFuture<List<Koodi>> ylakoodit(String koodiUri) {
    String[] parts = koodiUri.split("#");
    Map<String, String> parameters = new HashMap<>();
    if (parts.length > 1) {
      parameters.put("koodiVersio", parts[1]);
    }
    return this.client.getJson(
        this.urlConfiguration.url("koodisto-service.json.koodi.ylakoodit", parts[0], parameters),
        Duration.ofMinutes(1),
        new TypeToken<List<Koodi>>() {}.getType());
  }

  @Override
  public CompletableFuture<List<Koodi>> alakoodit(String koodiUri) {
    String[] parts = koodiUri.split("#");
    Map<String, String> parameters = new HashMap<>();
    if (parts.length > 1) {
      parameters.put("koodiVersio", parts[1]);
    }
    return this.client.getJson(
        this.urlConfiguration.url("koodisto-service.json.koodi.alakoodit", parts[0], parameters),
        Duration.ofMinutes(1),
        new TypeToken<List<Koodi>>() {}.getType());
  }

  @Override
  public CompletableFuture<Koodi> maatjavaltiot2ToMaatjavaltiot1(String koodiUri) {
    HashMap<String, Integer> query = new HashMap<>();
    query.put("koodiVersio", 1);
    return this.client
        .<List<Koodi>>getJson(
            this.urlConfiguration.url("koodisto-service.json.koodi.rinnasteinen", koodiUri, query),
            Duration.ofMinutes(1),
            new TypeToken<List<Koodi>>() {}.getType())
        .thenComposeAsync(
            koodit -> {
              Optional<Koodi> koodi =
                  koodit.stream()
                      .filter(k -> k.getKoodistoUri().equals("maatjavaltiot1"))
                      .findFirst();
              if (koodi.isPresent()) {
                return CompletableFuture.completedFuture(koodi.get());
              } else {
                LOG.warn(
                    String.format(
                        "Could not find related maatjavaltiot1 koodi for %s, returning maatjavaltiot1_xxx instead",
                        koodiUri));
                return haeKoodi("maatjavaltiot1_xxx");
              }
            });
  }

  private CompletableFuture<Koodi> haeKoodi(String koodiUri) {
    return this.client.getJson(
        this.urlConfiguration.url("koodisto-service.json.koodi", koodiUri),
        Duration.ofMinutes(1),
        new TypeToken<Koodi>() {}.getType());
  }

  @Override
  public CompletableFuture<List<Koodi>> haeKoodienUusinVersio(String... koodiUris) {
    return this.client.getJson(
        this.urlConfiguration.url("baseurl-koodisto-service")
            + "/koodisto-service/rest/json/searchKoodis?koodiUris="
            + Arrays.stream(koodiUris).collect(Collectors.joining(","))
            + "&koodiversioSelection=LATEST",
        Duration.ofMinutes(1),
        new TypeToken<List<Koodi>>() {}.getType());
  }
}
