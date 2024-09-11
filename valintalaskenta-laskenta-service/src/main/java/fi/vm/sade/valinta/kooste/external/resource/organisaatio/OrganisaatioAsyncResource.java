package fi.vm.sade.valinta.kooste.external.resource.organisaatio;

import fi.vm.sade.organisaatio.resource.dto.HakutoimistoDTO;
import fi.vm.sade.valinta.kooste.external.resource.organisaatio.dto.Organisaatio;
import fi.vm.sade.valinta.kooste.external.resource.organisaatio.dto.OrganisaatioTyyppiHierarkia;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;

public interface OrganisaatioAsyncResource {
  CompletableFuture<Organisaatio> haeOrganisaatio(String organisaatioOid);

  CompletableFuture<OrganisaatioTyyppiHierarkia> haeOrganisaationTyyppiHierarkia(
      String organisaatioOid);

  CompletableFuture<Optional<HakutoimistoDTO>> haeHakutoimisto(String organisaatioId);

  CompletableFuture<String> parentoids(String organisaatioId) throws Exception;
}
