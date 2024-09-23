package fi.vm.sade.valinta.kooste.external.resource.koodisto.impl;

import com.google.gson.reflect.TypeToken;
import fi.vm.sade.valinta.kooste.external.resource.HttpClient;
import fi.vm.sade.valinta.kooste.external.resource.UrlConfiguration;
import fi.vm.sade.valinta.kooste.external.resource.koodisto.KoodistoAsyncResource;
import fi.vm.sade.valinta.kooste.external.resource.koodisto.dto.Koodi;
import java.time.Duration;
import java.util.*;
import java.util.concurrent.CompletableFuture;
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
}
