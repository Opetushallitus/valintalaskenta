package fi.vm.sade.valintalaskenta.runner.resource.external.organisaatio;

import java.util.concurrent.CompletableFuture;

public interface OrganisaatioAsyncResource {

  CompletableFuture<String> parentoids(String organisaatioId) throws Exception;
}
