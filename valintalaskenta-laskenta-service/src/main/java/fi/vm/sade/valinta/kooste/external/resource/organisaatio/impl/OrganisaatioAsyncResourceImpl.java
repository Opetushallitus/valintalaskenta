package fi.vm.sade.valinta.kooste.external.resource.organisaatio.impl;

import com.google.gson.reflect.TypeToken;
import fi.vm.sade.organisaatio.resource.dto.HakutoimistoDTO;
import fi.vm.sade.valinta.kooste.external.resource.HttpClient;
import fi.vm.sade.valinta.kooste.external.resource.organisaatio.OrganisaatioAsyncResource;
import fi.vm.sade.valinta.kooste.external.resource.organisaatio.dto.Organisaatio;
import fi.vm.sade.valinta.kooste.external.resource.organisaatio.dto.OrganisaatioTyyppiHierarkia;
import fi.vm.sade.valinta.kooste.url.UrlConfiguration;
import java.io.IOException;
import java.time.Duration;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import org.apache.commons.io.IOUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

@Service
public class OrganisaatioAsyncResourceImpl implements OrganisaatioAsyncResource {
  private final HttpClient client;
  private final UrlConfiguration urlConfiguration;

  @Autowired
  public OrganisaatioAsyncResourceImpl(@Qualifier("OrganisaatioHttpClient") HttpClient client) {
    this.client = client;
    this.urlConfiguration = UrlConfiguration.getInstance();
  }

  @Override
  public CompletableFuture<Organisaatio> haeOrganisaatio(String organisaatioOid) {
    return this.client.getJson(
        this.urlConfiguration.url("organisaatio-service.organisaatio", organisaatioOid),
        Duration.ofMinutes(1),
        new TypeToken<Organisaatio>() {}.getType());
  }

  @Override
  public CompletableFuture<OrganisaatioTyyppiHierarkia> haeOrganisaationTyyppiHierarkia(
      String organisaatioOid) {
    Map<String, String> parameters = new HashMap<>();
    parameters.put("oid", organisaatioOid);
    parameters.put("aktiiviset", Boolean.toString(true));
    parameters.put("suunnitellut", Boolean.toString(true));
    parameters.put("lakkautetut", Boolean.toString(true));

    return client.getJson(
        this.urlConfiguration.url("organisaatio-service.organisaatio.hierarkia.tyyppi", parameters),
        Duration.ofMinutes(1),
        new TypeToken<OrganisaatioTyyppiHierarkia>() {}.getType());
  }

  @Override
  public CompletableFuture<Optional<HakutoimistoDTO>> haeHakutoimisto(String organisaatioId) {
    return this.client
        .getResponse(
            this.urlConfiguration.url(
                "organisaatio-service.organisaatio.hakutoimisto", organisaatioId),
            Duration.ofMinutes(1),
            x -> x)
        .thenApply(
            response -> {
              if (response.statusCode() == 404) {
                return Optional.empty();
              }
              return Optional.of(
                  this.client.parseJson(response, new TypeToken<HakutoimistoDTO>() {}.getType()));
            });
  }

  @Override
  public CompletableFuture<String> parentoids(String organisaatioId) throws Exception {
    return this.client
        .getResponse(
            this.urlConfiguration.url("valintalaskentakoostepalvelu.organisaatioService.rest.url")
                + "/organisaatio/"
                + organisaatioId
                + "/parentoids",
            Duration.ofMinutes(1),
            x -> x)
        .thenApply(
            r -> {
              try {
                return IOUtils.toString(r.body(), "UTF-8");
              } catch (IOException e) {
                throw new RuntimeException(e);
              }
            });
  }
}
