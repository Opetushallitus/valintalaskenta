package fi.vm.sade.valinta.kooste.external.resource.tarjonta;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CompletableFuture;

public interface TarjontaAsyncResource {
  CompletableFuture<Haku> haeHaku(String hakuOid);

  CompletableFuture<Set<String>> hakukohdeSearchByOrganizationGroupOids(
      Iterable<String> organizationGroupOids);

  CompletableFuture<Set<String>> hakukohdeSearchByOrganizationOids(
      Iterable<String> organizationOids);

  /**
   * Fetch from tarjonta-service the hakuOids that should be synchronized.
   *
   * @return Set of hakuOids as strings.
   */
  CompletableFuture<Map<String, List<String>>> hakukohdeRyhmasForHakukohdes(String hakuOid);
}
