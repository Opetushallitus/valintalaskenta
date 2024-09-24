package fi.vm.sade.valinta.kooste.external.resource.organisaatio.impl;

import fi.vm.sade.valinta.kooste.external.resource.HttpClient;
import fi.vm.sade.valinta.kooste.external.resource.UrlConfiguration;
import fi.vm.sade.valinta.kooste.external.resource.organisaatio.OrganisaatioAsyncResource;
import java.io.IOException;
import java.time.Duration;
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
