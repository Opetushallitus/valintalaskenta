package fi.vm.sade.valintalaskenta.tulos.resource.external;

import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import java.util.List;

public interface ValintaperusteetResource {

  List<ValintaperusteetDTO> haeValintaperusteet(final String hakukohdeOid, final Integer vaihe);
}
