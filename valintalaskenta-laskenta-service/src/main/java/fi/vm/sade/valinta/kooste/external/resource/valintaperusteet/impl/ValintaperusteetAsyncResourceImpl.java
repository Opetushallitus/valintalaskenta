package fi.vm.sade.valinta.kooste.external.resource.valintaperusteet.impl;

import com.google.gson.reflect.TypeToken;
import fi.vm.sade.service.valintaperusteet.dto.*;
import fi.vm.sade.valinta.kooste.external.resource.UrlConfiguration;
import fi.vm.sade.valinta.kooste.external.resource.valintaperusteet.ValintaperusteetAsyncResource;
import fi.vm.sade.valinta.kooste.external.resource.RestCasClient;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
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

  @Override
  public CompletableFuture<List<HakukohdeViiteDTO>> haunHakukohteet(String hakuOid) {
    return this.httpClient.get(
        this.urlConfiguration.url("valintaperusteet-service.valintalaskentakoostepalvelu.hakukohde.haku", hakuOid),
        new TypeToken<List<HakukohdeViiteDTO>>() {},
        Collections.emptyMap(),
        10 * 60 * 1000);
  }

  @Override
  public CompletableFuture<String> haeValintaryhmaVastuuorganisaatio(String valintaryhmaOid) {
    String url =
        this.urlConfiguration.url(
            "valintaperusteet-service.valintalaskentakoostepalvelu.valintaryhma.vastuuorganisaatio",
            valintaryhmaOid);
    LOG.info("Calling url {}", url);
    return this.httpClient
        .get(url, Map.of("Accept", "text/plain"), 10 * 60 * 1000)
        .thenApply(response -> response.getResponseBody());
  }

  @Override
  public CompletableFuture<List<ValintaperusteetDTO>> haeValintaperusteet(
      String hakukohdeOid, Optional<Integer> valinnanVaiheJarjestysluku) {
    List<Object> parameters = new LinkedList<>();
    parameters.add(hakukohdeOid);
    if (valinnanVaiheJarjestysluku.isPresent()) {
      Map<String, String> vaiheParameter = new HashMap<>();
      vaiheParameter.put("vaihe", valinnanVaiheJarjestysluku.get().toString());
      parameters.add(vaiheParameter);
    }

    String url =
        this.urlConfiguration.url(
            "valintaperusteet-service.valintalaskentakoostepalvelu.valintaperusteet",
            parameters.toArray());

    return httpClient.get(
        url, new TypeToken<List<ValintaperusteetDTO>>() {}, Collections.emptyMap(), 60 * 60 * 1000);
  }
}
