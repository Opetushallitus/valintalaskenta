package fi.vm.sade.valintalaskenta.runner.resource.external.koostepalvelu;

import fi.vm.sade.valintalaskenta.domain.dto.LaskeDTO;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.LaskentaDto;

public interface KoostepalveluAsyncResource {

  LaskeDTO haeLahtotiedot(
      LaskentaDto laskenta,
      String hakukohdeOid,
      boolean retryHakemuksetAndOppijat,
      boolean withHakijaRyhmat);
}
