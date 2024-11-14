package fi.vm.sade.valintalaskenta.runner.resource.external.koostepalvelu;

import fi.vm.sade.valintalaskenta.domain.dto.LaskeDTO;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.LaskentaDto;

import java.util.concurrent.CompletableFuture;

public interface KoostepalveluAsyncResource {

  CompletableFuture<LaskeDTO> haeLahtotiedot(
      LaskentaDto laskenta,
      String hakukohdeOid,
      boolean retryHakemuksetAndOppijat,
      boolean withHakijaRyhmat);
}
