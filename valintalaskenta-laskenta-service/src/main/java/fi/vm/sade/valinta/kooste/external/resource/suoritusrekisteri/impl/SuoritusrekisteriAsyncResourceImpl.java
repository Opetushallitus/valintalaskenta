package fi.vm.sade.valinta.kooste.external.resource.suoritusrekisteri.impl;

import com.google.common.collect.Lists;
import com.google.gson.reflect.TypeToken;
import fi.vm.sade.valinta.kooste.external.resource.UrlConfiguration;
import fi.vm.sade.valinta.kooste.external.resource.suoritusrekisteri.SuoritusrekisteriAsyncResource;
import fi.vm.sade.valinta.kooste.external.resource.suoritusrekisteri.dto.Arvosana;
import fi.vm.sade.valinta.kooste.external.resource.suoritusrekisteri.dto.Oppija;
import fi.vm.sade.valinta.kooste.external.resource.suoritusrekisteri.dto.Suoritus;
import fi.vm.sade.valinta.kooste.external.resource.viestintapalvelu.RestCasClient;
import fi.vm.sade.valinta.kooste.util.CompletableFutureUtil;
import io.reactivex.Observable;
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
  public Observable<List<Oppija>> getOppijatByHakukohde(String hakukohdeOid, String hakuOid) {
    return Observable.fromFuture(
        this.httpClient.get(
            this.urlConfiguration.url("suoritusrekisteri.oppijat")
                + "?hakukohde="
                + hakukohdeOid
                + "&haku="
                + hakuOid,
            new TypeToken<List<Oppija>>() {},
            Collections.emptyMap(),
            10 * 60 * 1000));
  }

  @Override
  public CompletableFuture<List<Oppija>> getOppijatByHakukohdeWithoutEnsikertalaisuus(
      String hakukohdeOid, String hakuOid) {
    Map<String, String> parameters = new HashMap<>();
    parameters.put("hakukohde", hakukohdeOid);
    parameters.put("haku", hakuOid);
    parameters.put("ensikertalaisuudet", "false");
    String url = this.urlConfiguration.url("suoritusrekisteri.oppijat", parameters);

    return httpClient.get(
        url, new TypeToken<List<Oppija>>() {}, Collections.emptyMap(), 5 * 60 * 1000);
  }

  @Override
  public Observable<Oppija> getSuorituksetByOppija(String opiskelijaOid, String hakuOid) {
    String uri =
        this.urlConfiguration.url("suoritusrekisteri.oppijat.opiskelijaoid", opiskelijaOid)
            + "?haku="
            + hakuOid;
    LOG.info("Calling url {}", uri);
    return Observable.fromFuture(
        this.httpClient.get(
            uri, new TypeToken<Oppija>() {}, Collections.emptyMap(), 10 * 60 * 1000));
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

  @Override
  public Observable<Oppija> getSuorituksetWithoutEnsikertalaisuus(String opiskelijaOid) {
    String url =
        this.urlConfiguration.url("suoritusrekisteri.oppijat.opiskelijaoid", opiskelijaOid);
    LOG.info("Calling url {}", url);
    return Observable.fromFuture(
        this.httpClient.get(
            url, new TypeToken<Oppija>() {}, Collections.emptyMap(), 10 * 60 * 1000));
  }

  @Override
  public CompletableFuture<List<Oppija>> getSuorituksetForOppijasWithoutEnsikertalaisuus(
      List<String> opiskelijaOids) {
    Map<String, String> parameters = new HashMap<>();
    parameters.put("ensikertalaisuudet", "false");
    String url = this.urlConfiguration.url("suoritusrekisteri.oppijat", parameters);
    return batchedPostOppijasFuture(opiskelijaOids, url);
  }

  @Override
  public Observable<List<Oppija>> getSuorituksetWithoutEnsikertalaisuus(
      List<String> opiskelijaOids) {
    String url =
        this.urlConfiguration.url("suoritusrekisteri.oppijat") + "/?ensikertalaisuudet=false";
    return batchedPostOppijas(opiskelijaOids, url);
  }

  private Observable<List<Oppija>> batchedPostOppijas(List<String> opiskelijaOids, String url) {
    if (opiskelijaOids.isEmpty()) {
      LOG.info(
          "Batched POST: empty list of oids provided. Returning an empty set without api call.");
      return Observable.just(Collections.emptyList());
    }
    List<List<String>> oidBatches = Lists.partition(opiskelijaOids, maxOppijatPostSize);
    LOG.info(
        "Batched POST: {} oids partitioned into {} batches",
        opiskelijaOids.size(),
        oidBatches.size());

    Observable<Observable<List<Oppija>>> obses =
        Observable.fromIterable(oidBatches)
            .map(
                oidBatch -> {
                  LOG.info("Calling POST url {} with {} opiskelijaOids", url, oidBatch.size());

                  return Observable.fromFuture(
                      this.httpClient.post(
                          url,
                          new TypeToken<List<Oppija>>() {},
                          oidBatch,
                          Collections.emptyMap(),
                          10 * 60 * 1000));
                });

    // Add the elements returned by each response to one master list
    Observable<List<Oppija>> allOppijas = Observable.concat(obses);
    return allOppijas;
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

  @Override
  public CompletableFuture<Suoritus> postSuoritus(Suoritus suoritus) {
    return httpClient.post(
        this.urlConfiguration.url("suoritusrekisteri.suoritukset"),
        new TypeToken<Suoritus>() {},
        suoritus,
        Collections.emptyMap(),
        10 * 60 * 1000);
  }

  @Override
  public CompletableFuture<Arvosana> postArvosana(Arvosana arvosana) {
    return httpClient.post(
        this.urlConfiguration.url("suoritusrekisteri.arvosanat"),
        new TypeToken<Arvosana>() {},
        arvosana,
        Collections.emptyMap(),
        10 * 6 * 1000);
  }

  @Override
  public CompletableFuture<Arvosana> updateExistingArvosana(
      String arvosanaId, Arvosana arvosanaWithUpdatedValues) {

    return httpClient.post(
        this.urlConfiguration.url("suoritusrekisteri.arvosanat.id", arvosanaId),
        new TypeToken<Arvosana>() {},
        arvosanaWithUpdatedValues,
        Collections.emptyMap(),
        10 * 60 * 1000);
  }

  @Override
  public CompletableFuture<String> deleteSuoritus(String suoritusId) {
    return httpClient.delete(
        this.urlConfiguration.url("suoritusrekisteri.suoritukset.id", suoritusId),
        Map.of("Accept", "*/*"),
        60 * 1000);
  }

  @Override
  public CompletableFuture<String> deleteArvosana(String arvosanaId) {
    return httpClient.delete(
        this.urlConfiguration.url("suoritusrekisteri.arvosanat.id", arvosanaId),
        Map.of("Accept", "*/*"),
        60 * 1000);
  }
}
