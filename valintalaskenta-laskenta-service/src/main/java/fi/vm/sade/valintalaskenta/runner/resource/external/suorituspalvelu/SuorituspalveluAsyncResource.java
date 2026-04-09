package fi.vm.sade.valintalaskenta.runner.resource.external.suorituspalvelu;

import fi.vm.sade.valintalaskenta.domain.dto.SuorituspalveluValintadataDTO;

public interface SuorituspalveluAsyncResource {
  SuorituspalveluValintadataDTO haeValintaData(String hakuOid, String hakukohdeOid);
}
