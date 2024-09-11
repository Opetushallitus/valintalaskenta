package fi.vm.sade.valinta.kooste.external.resource.tarjonta;

import fi.vm.sade.valinta.kooste.external.resource.tarjonta.dto.HakukohdeValintaperusteetDTO;
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

  CompletableFuture<AbstractHakukohde> haeHakukohde(String hakukohdeOid);

  CompletableFuture<Set<String>> haunHakukohteet(String hakuOid);

  CompletableFuture<Toteutus> haeToteutus(String toteutusOid);

  CompletableFuture<Koulutus> haeKoulutus(String koulutusOid);

  /**
   * Fetch from tarjonta-service the hakuOids that should be synchronized.
   *
   * @return Set of hakuOids as strings.
   */
  CompletableFuture<Set<String>> findHakuOidsForAutosyncTarjonta();

  CompletableFuture<Map<String, List<String>>> haunHakukohderyhmatCached(String hakuOid);

  CompletableFuture<Map<String, List<String>>> hakukohdeRyhmasForHakukohdes(String hakuOid);

  CompletableFuture<List<String>> hakukohdeRyhmasForHakukohde(String hakukohdeOid);

  CompletableFuture<HakukohdeValintaperusteetDTO> findValintaperusteetByOid(String hakukohdeOid);
}
