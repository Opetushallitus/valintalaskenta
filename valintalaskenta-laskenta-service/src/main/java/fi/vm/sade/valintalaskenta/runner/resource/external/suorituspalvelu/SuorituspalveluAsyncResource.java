package fi.vm.sade.valintalaskenta.runner.resource.external.suorituspalvelu;

import fi.vm.sade.valintalaskenta.domain.dto.SuorituspalveluValintadataDTO;
import java.util.concurrent.CompletableFuture;

public interface SuorituspalveluAsyncResource {
  CompletableFuture<SuorituspalveluValintadataDTO> haeValintaData(
      String hakuOid, String hakukohdeOid);
}
