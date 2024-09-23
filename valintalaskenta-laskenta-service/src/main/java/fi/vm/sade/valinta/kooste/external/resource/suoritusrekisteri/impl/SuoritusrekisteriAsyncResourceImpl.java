package fi.vm.sade.valinta.kooste.external.resource.suoritusrekisteri.impl;

import com.google.common.collect.Lists;
import com.google.gson.reflect.TypeToken;
import fi.vm.sade.valinta.kooste.external.resource.UrlConfiguration;
import fi.vm.sade.valinta.kooste.external.resource.suoritusrekisteri.SuoritusrekisteriAsyncResource;
import fi.vm.sade.valinta.kooste.external.resource.suoritusrekisteri.dto.Oppija;
import fi.vm.sade.valinta.kooste.external.resource.RestCasClient;
import fi.vm.sade.valinta.kooste.util.CompletableFutureUtil;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

@Service
public class SuoritusrekisteriAsyncResourceImpl implements SuoritusrekisteriAsyncResource {
  private static final Logger LOG =
      LoggerFactory.getLogger(SuoritusrekisteriAsyncResourceImpl.class);
  private final RestCasClient httpClient;

  private final UrlConfiguration urlConfiguration;

  private int maxOppijatPostSize = 5000;

  @Autowired
  public SuoritusrekisteriAsyncResourceImpl(
      @Qualifier("SuoritusrekisteriCasClient") RestCasClient httpClient) {
    this.httpClient = httpClient;
    this.urlConfiguration = UrlConfiguration.getInstance();
  }

  @Override
  public CompletableFuture<List<Oppija>> getSuorituksetByOppijas(
      List<String> opiskelijaOids, String hakuOid) {
    Map<String, String> parameters = new HashMap<>();
    parameters.put("ensikertalaisuudet", "true");
    parameters.put("haku", hakuOid);
    String url = this.urlConfiguration.url("suoritusrekisteri.oppijat", parameters);
    return batchedPostOppijasFuture(opiskelijaOids, url);
  }

  private CompletableFuture<List<Oppija>> batchedPostOppijasFuture(
      List<String> opiskelijaOids, String url) {
    if (opiskelijaOids.isEmpty()) {
      LOG.info(
          "Batched POST: empty list of oids provided. Returning an empty set without api call.");
      return CompletableFuture.completedFuture(Collections.emptyList());
    }
    List<List<String>> oidBatches = Lists.partition(opiskelijaOids, maxOppijatPostSize);
    LOG.info(
        "Batched POST: {} oids partitioned into {} batches",
        opiskelijaOids.size(),
        oidBatches.size());

    return CompletableFutureUtil.sequence(
            oidBatches.stream()
                .map(
                    oidBatch -> {
                      LOG.info("Calling POST url {} with {} opiskelijaOids", url, oidBatch.size());

                      return httpClient.post(
                          url,
                          new TypeToken<List<Oppija>>() {},
                          oidBatch,
                          Collections.emptyMap(),
                          5 * 60 * 1000);
                    })
                .collect(Collectors.toList()))
        .thenApplyAsync(
            (List<List<Oppija>> r) ->
                r.stream().flatMap(List::stream).collect(Collectors.toList()));
  }
}
