package fi.vm.sade.valinta.kooste.external.resource.hakuapp.impl;

import com.google.gson.reflect.TypeToken;
import fi.vm.sade.valinta.kooste.external.resource.UrlConfiguration;
import fi.vm.sade.valinta.kooste.external.resource.hakuapp.ApplicationAsyncResource;
import fi.vm.sade.valinta.kooste.external.resource.hakuapp.dto.*;
import fi.vm.sade.valinta.kooste.external.resource.RestCasClient;
import fi.vm.sade.valinta.kooste.util.HakemusWrapper;
import fi.vm.sade.valinta.kooste.util.HakuappHakemusWrapper;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

@Service
public class ApplicationAsyncResourceImpl implements ApplicationAsyncResource {
  private final Logger LOG = LoggerFactory.getLogger(getClass());
  private final RestCasClient client;

  private final UrlConfiguration urlConfiguration;

  @Autowired
  public ApplicationAsyncResourceImpl(@Qualifier("HakuAppCasClient") RestCasClient client) {
    this(client, UrlConfiguration.getInstance());
  }

  ApplicationAsyncResourceImpl(RestCasClient client, UrlConfiguration urlConfiguration) {
    this.client = client;
    this.urlConfiguration = urlConfiguration;
  }

  @Override
  public CompletableFuture<List<HakemusWrapper>> getApplicationsByOids(
      String hakuOid, Collection<String> hakukohdeOids) {
    HashMap<String, Object> query = new HashMap<>();
    query.put("appState", DEFAULT_STATES);
    query.put("rows", DEFAULT_ROW_LIMIT);
    query.put("asId", hakuOid);
    query.put("aoOid", hakukohdeOids);
    String url = this.urlConfiguration.url("haku-app.applications.listfull", query);
    LOG.info("Calling url {}", url);

    return this.client
        .get(url, new TypeToken<List<Hakemus>>() {}, Collections.emptyMap(), 60 * 60 * 1000)
        .thenApplyAsync(
            hs -> hs.stream().map(HakuappHakemusWrapper::new).collect(Collectors.toList()));
  }
}
