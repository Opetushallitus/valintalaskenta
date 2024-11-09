package fi.vm.sade.valinta.kooste.external.resource.organisaatio;

import java.util.concurrent.CompletableFuture;

public interface OrganisaatioAsyncResource {

  CompletableFuture<String> parentoids(String organisaatioId) throws Exception;
}
