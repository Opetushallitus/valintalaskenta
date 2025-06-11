package fi.vm.sade.valintalaskenta.runner.resource.external.tarjonta;

import java.util.Set;
import java.util.concurrent.CompletableFuture;

public interface TarjontaAsyncResource {
  CompletableFuture<Set<String>> haeTarjoajaOids(String hakuOid);

  CompletableFuture<Set<String>> hakukohdeSearchByOrganizationGroupOids(
      Iterable<String> organizationGroupOids);

  CompletableFuture<Set<String>> hakukohdeSearchByOrganizationOids(
      Iterable<String> organizationOids);
}
