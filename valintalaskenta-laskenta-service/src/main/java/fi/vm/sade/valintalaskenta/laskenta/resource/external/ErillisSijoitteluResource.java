package fi.vm.sade.valintalaskenta.laskenta.resource.external;

import fi.vm.sade.sijoittelu.tulos.dto.ValisijoitteluDTO;

public interface ErillisSijoitteluResource {
  Long sijoittele(final String hakuOid, final ValisijoitteluDTO hakukohteet);
}
