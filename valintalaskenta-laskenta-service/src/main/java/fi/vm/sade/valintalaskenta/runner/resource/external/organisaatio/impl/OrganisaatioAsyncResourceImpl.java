package fi.vm.sade.valintalaskenta.runner.resource.external.organisaatio.impl;

import fi.vm.sade.valintalaskenta.runner.resource.external.RunnerHttpClient;
import fi.vm.sade.valintalaskenta.runner.resource.external.UrlConfiguration;
import fi.vm.sade.valintalaskenta.runner.resource.external.organisaatio.OrganisaatioAsyncResource;
import java.io.IOException;
import java.time.Duration;
import java.util.concurrent.CompletableFuture;
import org.apache.commons.io.IOUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

@Service
public class OrganisaatioAsyncResourceImpl implements OrganisaatioAsyncResource {
  private final RunnerHttpClient client;
  private final UrlConfiguration urlConfiguration;

  @Autowired
  public OrganisaatioAsyncResourceImpl(
      @Qualifier("OrganisaatioHttpClient") RunnerHttpClient client) {
    this.client = client;
    this.urlConfiguration = UrlConfiguration.getInstance();
  }

  @Override
  public CompletableFuture<String> parentoids(String organisaatioId) {
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
