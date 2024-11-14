package fi.vm.sade.valintalaskenta.runner.resource.external.tarjonta.impl;

import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;
import com.google.gson.Gson;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonObject;
import com.google.gson.reflect.TypeToken;
import fi.vm.sade.tarjonta.service.resources.v1.dto.*;
import fi.vm.sade.tarjonta.service.resources.v1.dto.koulutus.KoulutusV1RDTO;
import fi.vm.sade.valintalaskenta.runner.resource.external.HttpClient;
import fi.vm.sade.valintalaskenta.runner.resource.external.UrlConfiguration;
import fi.vm.sade.valintalaskenta.runner.resource.external.tarjonta.dto.KoutaHakukohdeDTO;
import fi.vm.sade.valintalaskenta.runner.resource.external.tarjonta.*;
import fi.vm.sade.valintalaskenta.runner.resource.external.tarjonta.dto.*;
import fi.vm.sade.valintalaskenta.runner.resource.external.RestCasClient;
import fi.vm.sade.valinta.sharedutils.http.DateDeserializer;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.time.Duration;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import fi.vm.sade.valintalaskenta.runner.resource.external.tarjonta.TarjontaAsyncResource;
import fi.vm.sade.valintalaskenta.runner.resource.external.tarjonta.dto.KoutaHaku;
import fi.vm.sade.valintalaskenta.runner.resource.external.tarjonta.dto.ResultHakukohde;
import fi.vm.sade.valintalaskenta.runner.resource.external.tarjonta.dto.ResultSearch;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

@Service
public class TarjontaAsyncResourceImpl implements TarjontaAsyncResource {
  private final UrlConfiguration urlConfiguration = UrlConfiguration.getInstance();
  private final HttpClient client;
  private final RestCasClient koutaClient;
  private final RestCasClient hakukohderyhmapalveluClient;

  private final Integer KOUTA_OID_LENGTH = 35;

  private static final Logger LOG = LoggerFactory.getLogger(TarjontaAsyncResourceImpl.class);

  private final Cache<String, CompletableFuture<List<String>>> hakukohderyhmanHakukohteetCache =
      CacheBuilder.newBuilder().expireAfterAccess(1, TimeUnit.HOURS).build();

  @Autowired
  public TarjontaAsyncResourceImpl(
      @Qualifier("TarjontaHttpClient") HttpClient client,
      @Qualifier("KoutaCasClient") RestCasClient koutaClient,
      @Qualifier("HakukohderyhmapalveluCasClient") RestCasClient hakukohderyhmapalveluClient) {
    this.client = client;
    this.koutaClient = koutaClient;
    this.hakukohderyhmapalveluClient = hakukohderyhmapalveluClient;
  }

  private static <T> CompletableFuture<List<T>> sequence(List<CompletableFuture<T>> fs) {
    return CompletableFuture.allOf(fs.toArray(CompletableFuture[]::new))
        .thenApplyAsync(v -> fs.stream().map(CompletableFuture::join).collect(Collectors.toList()));
  }

  @Override
  public CompletableFuture<Set<String>> hakukohdeSearchByOrganizationGroupOids(
      Iterable<String> organizationGroupOids) {

    Map<String, String[]> tarjontaParams = new HashMap<>();
    tarjontaParams.put(
        "organisaatioRyhmaOid",
        StreamSupport.stream(organizationGroupOids.spliterator(), false).toArray(String[]::new));
    CompletableFuture<Set<String>> fromTarjonta =
        this.client
            .<ResultSearch>getJson(
                urlConfiguration.url("tarjonta-service.hakukohde.search", tarjontaParams),
                Duration.ofMinutes(5),
                new TypeToken<ResultSearch>() {}.getType())
            .thenApplyAsync(
                r ->
                    r.getResult().getTulokset().stream()
                        .flatMap(t -> t.getTulokset().stream())
                        .map(ResultHakukohde::getOid)
                        .collect(Collectors.toSet()));

    CompletableFuture<List<List<String>>> fromHakukohderyhmapalvelu =
        sequence(
            StreamSupport.stream(organizationGroupOids.spliterator(), false)
                .map(this::hakukohderyhmanHakukohteet)
                .collect(Collectors.toList()));

    return fromTarjonta.thenCombine(
        fromHakukohderyhmapalvelu,
        (t, h) -> {
          Set<String> result = new HashSet<>(t);
          for (List<String> oids : h) {
            result.addAll(oids);
          }
          return result;
        });
  }

  private CompletableFuture<List<String>> hakukohderyhmanHakukohteet(String hakukohderyhmaOid) {
    try {
      return hakukohderyhmanHakukohteetCache.get(
          hakukohderyhmaOid,
          () ->
              this.hakukohderyhmapalveluClient.get(
                  urlConfiguration.url(
                      "hakukohderyhmapalvelu.hakukohderyhman-hakukohteet", hakukohderyhmaOid),
                  new TypeToken<>() {},
                  Collections.emptyMap(),
                  60 * 1000));
    } catch (ExecutionException e) {
      LOG.error(
          "Hakukohderyhmän {} hakukohteiden haku epäonnistui: {}",
          hakukohderyhmaOid,
          e.getMessage());
      return CompletableFuture.failedFuture(e);
    }
  }

  @Override
  public CompletableFuture<Set<String>> hakukohdeSearchByOrganizationOids(
      Iterable<String> organizationOids) {
    Map<String, String[]> tarjontaParameters = new HashMap<>();
    tarjontaParameters.put(
        "organisationOid",
        StreamSupport.stream(organizationOids.spliterator(), false).toArray(String[]::new));
    CompletableFuture<Set<String>> tarjontaF =
        this.client
            .<ResultSearch>getJson(
                urlConfiguration.url("tarjonta-service.hakukohde.search", tarjontaParameters),
                Duration.ofMinutes(5),
                new TypeToken<ResultSearch>() {}.getType())
            .thenApplyAsync(
                r ->
                    r.getResult().getTulokset().stream()
                        .flatMap(t -> t.getTulokset().stream())
                        .map(ResultHakukohde::getOid)
                        .collect(Collectors.toSet()));
    CompletableFuture<List<Set<String>>> koutaF =
        sequence(
            StreamSupport.stream(organizationOids.spliterator(), false)
                .map(
                    organizationOid -> {
                      Map<String, String> koutaParameters = new HashMap<>();
                      koutaParameters.put("tarjoaja", organizationOid);
                      return this.koutaClient
                          .get(
                              urlConfiguration.url(
                                  "kouta-internal.hakukohde.search", koutaParameters),
                              new TypeToken<Set<KoutaHakukohdeDTO>>() {},
                              Collections.emptyMap(),
                              10 * 1000)
                          .thenApplyAsync(
                              hakukohteet ->
                                  hakukohteet.stream().map(h -> h.oid).collect(Collectors.toSet()));
                    })
                .collect(Collectors.toList()));
    return tarjontaF.thenComposeAsync(
        tarjontaHakukohdeOids ->
            koutaF.thenApplyAsync(
                koutaHakukohdeOids -> {
                  HashSet<String> s = new HashSet<>(tarjontaHakukohdeOids);
                  for (Set<String> oids : koutaHakukohdeOids) {
                    s.addAll(oids);
                  }
                  return s;
                }));
  }

  private CompletableFuture<HakuV1RDTO> getTarjontaHaku(String hakuOid) {
    return this.client
        .<ResultV1RDTO<HakuV1RDTO>>getJson(
            urlConfiguration.url("tarjonta-service.haku.hakuoid", hakuOid),
            Duration.ofMinutes(5),
            new TypeToken<ResultV1RDTO<HakuV1RDTO>>() {}.getType())
        .thenApplyAsync(ResultV1RDTO::getResult);
  }

  @Override
  public CompletableFuture<Set<String>> haeTarjoajaOids(String hakuOid) {
    if (KOUTA_OID_LENGTH.equals(hakuOid.length())) {
      CompletableFuture<KoutaHaku> koutaF =
          this.koutaClient.get(
              urlConfiguration.url("kouta-internal.haku.hakuoid", hakuOid),
              new TypeToken<KoutaHaku>() {},
              Collections.emptyMap(),
              10 * 1000);
      return koutaF.thenApplyAsync(h -> Set.of(h.organisaatioOid));
    } else {
      return this.getTarjontaHaku(hakuOid).thenApplyAsync(h -> Set.of(h.getTarjoajaOids()));
    }
  }

  public static Gson getGson() {
    return DateDeserializer.gsonBuilder()
        .registerTypeAdapter(
            KoulutusV1RDTO.class,
            (JsonDeserializer<KoulutusV1RDTO>)
                (json, typeOfT, context) -> {
                  JsonObject o = json.getAsJsonObject();
                  String toteutustyyppi = o.getAsJsonPrimitive("toteutustyyppi").getAsString();
                  for (JsonSubTypes.Type type :
                      KoulutusV1RDTO.class.getAnnotation(JsonSubTypes.class).value()) {
                    if (type.name().equals(toteutustyyppi)) {
                      return context.deserialize(o, type.value());
                    }
                  }
                  throw new IllegalStateException(
                      String.format(
                          "Tyyppiä %s olevan koulutuksen jäsentäminen epäonnistui",
                          toteutustyyppi));
                })
        .registerTypeAdapter(
            ResultV1RDTO.class,
            (JsonDeserializer)
                (json, typeOfT, context) -> {
                  Type accessRightsType = new TypeToken<Map<String, Boolean>>() {}.getType();
                  Type errorsType = new TypeToken<List<ErrorV1RDTO>>() {}.getType();
                  Type paramsType = new TypeToken<GenericSearchParamsV1RDTO>() {}.getType();
                  Type resultType = ((ParameterizedType) typeOfT).getActualTypeArguments()[0];
                  Type statusType = new TypeToken<ResultV1RDTO.ResultStatus>() {}.getType();
                  JsonObject o = json.getAsJsonObject();
                  ResultV1RDTO r = new ResultV1RDTO();
                  r.setAccessRights(context.deserialize(o.get("accessRights"), accessRightsType));
                  r.setErrors(context.deserialize(o.get("errors"), errorsType));
                  r.setParams(context.deserialize(o.get("params"), paramsType));
                  r.setResult(context.deserialize(o.get("result"), resultType));
                  r.setStatus(context.deserialize(o.get("status"), statusType));
                  return r;
                })
        .create();
  }
}
