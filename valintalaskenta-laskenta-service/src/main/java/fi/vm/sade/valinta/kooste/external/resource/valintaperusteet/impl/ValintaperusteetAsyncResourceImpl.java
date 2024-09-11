package fi.vm.sade.valinta.kooste.external.resource.valintaperusteet.impl;

import com.google.common.collect.Lists;
import com.google.gson.reflect.TypeToken;
import fi.vm.sade.service.valintaperusteet.dto.*;
import fi.vm.sade.valinta.kooste.external.resource.valintaperusteet.ValintaperusteetAsyncResource;
import fi.vm.sade.valinta.kooste.external.resource.viestintapalvelu.RestCasClient;
import fi.vm.sade.valinta.kooste.url.UrlConfiguration;
import io.reactivex.Observable;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Service
public class ValintaperusteetAsyncResourceImpl implements ValintaperusteetAsyncResource {
  private static final Logger LOG =
      LoggerFactory.getLogger(ValintaperusteetAsyncResourceImpl.class);
  private final RestCasClient httpClient;

  private final UrlConfiguration urlConfiguration;

  @Autowired
  public ValintaperusteetAsyncResourceImpl(
      @Qualifier("ValintaperusteetCasClient") RestCasClient httpClient) {
    this.httpClient = httpClient;
    this.urlConfiguration = UrlConfiguration.getInstance();
  }

  public Observable<List<ValinnanVaiheJonoillaDTO>> haeIlmanlaskentaa(String hakukohdeOid) {
    LOG.info("Valinnanvaiheiden haku...");
    return Observable.fromFuture(
        this.httpClient.get(
            this.urlConfiguration.url(
                "valintaperusteet-service.valintalaskentakoostepalvelu.hakukohde.ilmanlaskentaa",
                hakukohdeOid),
            new TypeToken<List<ValinnanVaiheJonoillaDTO>>() {},
            Collections.emptyMap(),
            10 * 60 * 1000));
  }

  public CompletableFuture<List<ValintaperusteetHakijaryhmaDTO>> haeHakijaryhmat(
      String hakukohdeOid) {
    return httpClient.get(
        this.urlConfiguration.url(
            "valintaperusteet-service.valintalaskentakoostepalvelu.valintaperusteet.hakijaryhma",
            hakukohdeOid),
        new TypeToken<List<ValintaperusteetHakijaryhmaDTO>>() {},
        Collections.emptyMap(),
        60 * 60 * 1000);
  }

  public CompletableFuture<List<ValintaperusteetDTO>> haeValintaperusteet(
      String hakukohdeOid, Integer valinnanVaiheJarjestysluku) {
    List<Object> parameters = new LinkedList<>();
    parameters.add(hakukohdeOid);
    if (valinnanVaiheJarjestysluku != null) {
      Map<String, String> vaiheParameter = new HashMap<>();
      vaiheParameter.put("vaihe", valinnanVaiheJarjestysluku.toString());
      parameters.add(vaiheParameter);
    }

    String url =
        this.urlConfiguration.url(
            "valintaperusteet-service.valintalaskentakoostepalvelu.valintaperusteet",
            parameters.toArray());

    return httpClient.get(
        url, new TypeToken<List<ValintaperusteetDTO>>() {}, Collections.emptyMap(), 60 * 60 * 1000);
  }

  public Observable<List<HakukohdeViiteDTO>> haunHakukohteet(String hakuOid) {
    return Observable.fromFuture(
        this.httpClient.get(
            this.urlConfiguration.url(
                "valintaperusteet-service.valintalaskentakoostepalvelu.hakukohde.haku", hakuOid),
            new TypeToken<List<HakukohdeViiteDTO>>() {},
            Collections.emptyMap(),
            10 * 60 * 1000));
  }

  @Override
  public Observable<ResponseEntity> tuoHakukohde(HakukohdeImportDTO hakukohde) {
    return Observable.fromFuture(
        this.httpClient
            .post(
                this.urlConfiguration.url(
                    "valintaperusteet-service.valintalaskentakoostepalvelu.valintaperusteet.tuohakukohde"),
                hakukohde,
                Collections.emptyMap(),
                10 * 60 * 1000)
            .thenApply(r -> ResponseEntity.status(r.getStatusCode()).build()));
  }

  @Override
  public CompletableFuture<List<ValintaperusteDTO>> findAvaimet(String hakukohdeOid) {
    return httpClient.get(
        this.urlConfiguration.url(
            "valintaperusteet-service.valintalaskentakoostepalvelu.hakukohde.avaimet.oid",
            hakukohdeOid),
        new TypeToken<List<ValintaperusteDTO>>() {},
        Collections.emptyMap(),
        60 * 60 * 1000);
  }

  @Override
  public Observable<List<HakukohdeJaValintaperusteDTO>> findAvaimet(
      Collection<String> hakukohdeOids) {
    return Observable.fromFuture(
        this.httpClient.post(
            this.urlConfiguration.url(
                "valintaperusteet-service.valintalaskentakoostepalvelu.hakukohde.avaimet"),
            new TypeToken<List<HakukohdeJaValintaperusteDTO>>() {},
            Lists.newArrayList(hakukohdeOids),
            Collections.emptyMap(),
            10 * 60 * 1000));
  }

  @Override
  public Observable<List<ValintaperusteetDTO>> valintaperusteet(String valinnanvaiheOid) {
    return Observable.fromFuture(
        this.httpClient.get(
            this.urlConfiguration.url(
                "valintaperusteet-service.valintalaskentakoostepalvelu.valinnanvaihe.valintaperusteet",
                valinnanvaiheOid),
            new TypeToken<List<ValintaperusteetDTO>>() {},
            Collections.emptyMap(),
            10 * 60 * 1000));
  }

  @Override
  public Observable<List<HakukohdeJaValintakoeDTO>> haeValintakokeetHakukohteille(
      Collection<String> hakukohdeOids) {
    return Observable.fromFuture(
        this.httpClient.post(
            this.urlConfiguration.url(
                "valintaperusteet-service.valintalaskentakoostepalvelu.hakukohde.valintakoe"),
            new TypeToken<List<HakukohdeJaValintakoeDTO>>() {},
            hakukohdeOids,
            Collections.emptyMap(),
            10 * 60 * 1000));
  }

  @Override
  public Observable<List<HakukohdeJaValintakoeDTO>> haeValintakokeetHakutoiveille(
      Collection<String> hakukohdeOids) {
    return Observable.fromFuture(
        this.httpClient.post(
            this.urlConfiguration.url(
                "valintaperusteet-service.valintalaskentakoostepalvelu.hakukohde.valintakoe"),
            new TypeToken<List<HakukohdeJaValintakoeDTO>>() {},
            Lists.newArrayList(hakukohdeOids),
            Collections.emptyMap(),
            10 * 60 * 1000));
  }

  @Override
  public Observable<Map<String, List<ValintatapajonoDTO>>> haeValintatapajonotSijoittelulle(
      Collection<String> hakukohdeOids) {
    return Observable.fromFuture(
        this.httpClient.post(
            this.urlConfiguration.url(
                "valintaperusteet-service.valintalaskentakoostepalvelu.valintatapajono"),
            new TypeToken<Map<String, List<ValintatapajonoDTO>>>() {},
            hakukohdeOids,
            Collections.emptyMap(),
            10 * 60 * 1000));
  }

  @Override
  public Observable<List<ValintakoeDTO>> haeValintakokeetHakukohteelle(String hakukohdeOid) {
    return Observable.fromFuture(
        this.httpClient.get(
            this.urlConfiguration.url(
                "valintaperusteet-service.valintalaskentakoostepalvelu.valintakoe", hakukohdeOid),
            new TypeToken<List<ValintakoeDTO>>() {},
            Collections.emptyMap(),
            10 * 60 * 1000));
  }

  @Override
  public Observable<Set<String>> haeHakukohteetValinnanvaiheelle(String oid) {
    String url =
        this.urlConfiguration.url(
            "valintaperusteet-service.valintalaskentakoostepalvelu.valinnanvaihe.hakukohteet", oid);
    LOG.info("Calling url {}", url);

    return Observable.fromFuture(
        this.httpClient.get(
            url, new TypeToken<Set<String>>() {}, Collections.emptyMap(), 10 * 60 * 1000));
  }

  @Override
  public Observable<String> haeValintaryhmaVastuuorganisaatio(String valintaryhmaOid) {
    String url =
        this.urlConfiguration.url(
            "valintaperusteet-service.valintalaskentakoostepalvelu.valintaryhma.vastuuorganisaatio",
            valintaryhmaOid);
    LOG.info("Calling url {}", url);
    return Observable.fromFuture(
        this.httpClient
            .get(url, Map.of("Accept", "text/plain"), 10 * 60 * 1000)
            .thenApply(response -> response.getResponseBody()));
  }
}
