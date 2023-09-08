package fi.vm.sade.valintalaskenta.laskenta.resource.external;

import fi.vm.sade.sijoittelu.tulos.dto.HakukohdeDTO;
import fi.vm.sade.sijoittelu.tulos.dto.ValisijoitteluDTO;
import java.util.List;

public interface ValiSijoitteluResource {
  List<HakukohdeDTO> sijoittele(final String hakuOid, final ValisijoitteluDTO hakukohteet);
}
