package fi.vm.sade.valinta.kooste.external.resource.hakuapp.impl;

import com.google.common.collect.Lists;
import com.google.gson.reflect.TypeToken;
import fi.vm.sade.valinta.kooste.external.resource.hakuapp.ApplicationAsyncResource;
import fi.vm.sade.valinta.kooste.external.resource.hakuapp.dto.*;
import fi.vm.sade.valinta.kooste.external.resource.viestintapalvelu.RestCasClient;
import fi.vm.sade.valinta.kooste.hakemus.dto.ApplicationOidsAndReason;
import fi.vm.sade.valinta.kooste.url.UrlConfiguration;
import fi.vm.sade.valinta.kooste.util.HakemusWrapper;
import fi.vm.sade.valinta.kooste.util.HakuappHakemusWrapper;
import io.reactivex.Observable;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;
import org.apache.commons.lang3.StringUtils;
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

  private List<HakemusWrapper> toHakemusWrapper(List<Hakemus> h) {
    return h.stream().map(HakuappHakemusWrapper::new).collect(Collectors.toList());
  }

  @Override
  public Observable<List<HakemusWrapper>> putApplicationPrototypes(
      String hakuOid,
      String hakukohdeOid,
      String tarjoajaOid,
      Collection<HakemusPrototyyppi> hakemusPrototyypit) {
    String url = this.urlConfiguration.url("haku-app.applications.syntheticapplication");
    HakemusPrototyyppiBatch hakemusPrototyyppiBatch =
        new HakemusPrototyyppiBatch(hakuOid, hakukohdeOid, tarjoajaOid, hakemusPrototyypit);

    return Observable.fromFuture(
        this.client
            .put(
                url,
                new TypeToken<List<Hakemus>>() {},
                hakemusPrototyyppiBatch,
                Collections.emptyMap(),
                60 * 60 * 1000)
            .thenApply(this::toHakemusWrapper));
  }

  @Override
  public CompletableFuture<List<HakemusWrapper>> getApplicationsByOid(
      String hakuOid, String hakukohdeOid) {
    return getApplicationsByOids(hakuOid, Collections.singletonList(hakukohdeOid));
  }

  @Override
  public Observable<Set<String>> getApplicationOids(String hakuOid, String hakukohdeOid) {
    ListFullSearchDTO s =
        new ListFullSearchDTO(
            "",
            Collections.singletonList(hakukohdeOid),
            Collections.singletonList(hakuOid),
            Collections.emptyList(),
            Collections.singletonList("oid"));
    return Observable.fromFuture(
        this.client
            .post(
                this.urlConfiguration.url("haku-app.applications.listfull"),
                new TypeToken<List<HakemusOid>>() {},
                s,
                Collections.emptyMap(),
                60 * 60 * 1000)
            .thenApply(l -> l.stream().map(HakemusOid::getOid).collect(Collectors.toSet())));
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

  @Override
  public CompletableFuture<List<HakemusWrapper>> getApplicationsByOidsWithPOST(
      String hakuOid, List<String> hakukohdeOids) {
    Map<String, List<String>> requestBody = new HashMap<>();
    requestBody.put("states", DEFAULT_STATES);
    requestBody.put("asIds", Collections.singletonList(hakuOid));
    requestBody.put("aoOids", hakukohdeOids);
    requestBody.put("keys", ApplicationAsyncResource.DEFAULT_KEYS);

    return this.client
        .post(
            this.urlConfiguration.url("haku-app.applications.listfull"),
            new TypeToken<List<Hakemus>>() {},
            requestBody,
            Collections.emptyMap(),
            60 * 60 * 1000)
        .thenApplyAsync(this::toHakemusWrapper);
  }

  @Override
  public Observable<List<HakemusWrapper>> getApplicationsByHakemusOids(List<String> hakemusOids) {
    return Observable.fromFuture(
        getApplicationsByHakemusOids(null, hakemusOids, Collections.emptyList()));
  }

  private CompletableFuture<List<HakemusWrapper>> getApplicationsByHakemusOids(
      String hakuOid, List<String> hakemusOids, List<String> keys) {
    HashMap<String, Object> query = new HashMap<>();
    query.put("rows", DEFAULT_ROW_LIMIT);
    if (!keys.isEmpty()) {
      query.put("asIds", hakuOid);
      query.put("state", DEFAULT_STATES);
      query.put("keys", keys);
    }

    return this.client
        .post(
            this.urlConfiguration.url("haku-app.applications.list", query),
            new TypeToken<List<Hakemus>>() {},
            hakemusOids,
            Collections.emptyMap(),
            60 * 60 * 1000)
        .thenApplyAsync(
            hs ->
                hs.stream()
                    .filter(
                        h ->
                            StringUtils.isEmpty(h.getState())
                                || DEFAULT_STATES.contains(h.getState()))
                    .map(HakuappHakemusWrapper::new)
                    .collect(Collectors.toList()));
  }

  @Override
  public CompletableFuture<List<HakemusWrapper>> getApplicationsByhakemusOidsInParts(
      String hakuOid, List<String> hakemusOids, List<String> keys) {
    List<CompletableFuture<List<HakemusWrapper>>> fs =
        Lists.partition(hakemusOids, DEFAULT_ROW_LIMIT).stream()
            .map(oids -> getApplicationsByHakemusOids(hakuOid, oids, keys))
            .collect(Collectors.toList());
    return CompletableFuture.allOf(fs.toArray(new CompletableFuture[0]))
        .thenApplyAsync(
            v -> fs.stream().flatMap(f -> f.join().stream()).collect(Collectors.toList()));
  }

  @Override
  public Observable<List<HakemusWrapper>> getApplicationsByOids(Collection<String> hakemusOids) {
    return Observable.fromFuture(
        this.client
            .post(
                this.urlConfiguration.url("haku-app.applications.list")
                    + "?rows="
                    + DEFAULT_ROW_LIMIT,
                new TypeToken<List<Hakemus>>() {},
                Lists.newArrayList(hakemusOids),
                Collections.emptyMap(),
                60 * 60 * 1000)
            .thenApply(this::toHakemusWrapper));
  }

  @Override
  public Observable<HakemusWrapper> getApplication(String hakemusOid) {
    return Observable.fromFuture(
        this.client
            .get(
                this.urlConfiguration.url("haku-app.applications", hakemusOid),
                new TypeToken<Hakemus>() {},
                Collections.emptyMap(),
                60 * 60 * 1000)
            .thenApply(HakuappHakemusWrapper::new));
  }

  @Override
  public Observable<String> changeStateOfApplicationsToPassive(
      List<String> hakemusOids, String reason) {
    return Observable.fromFuture(
        this.client
            .post(
                this.urlConfiguration.url("haku-app.applications.state.passivate"),
                new ApplicationOidsAndReason(hakemusOids, reason),
                Collections.emptyMap(),
                60 * 60 * 1000)
            .thenApply(r -> "OK"));
  }
}
