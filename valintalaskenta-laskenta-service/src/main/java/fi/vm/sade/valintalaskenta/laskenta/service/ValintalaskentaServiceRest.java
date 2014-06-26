package fi.vm.sade.valintalaskenta.laskenta.service;

import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;

import java.util.List;

/**
 * Created by jukais on 24.3.2014.
 */
public interface ValintalaskentaServiceRest {
    //	@PreAuthorize(CRUD)
    String laske(List<HakemusDTO> hakemus,
                 List<ValintaperusteetDTO> valintaperuste)
                throws RuntimeException;

    //	@PreAuthorize(CRUD)
    String valintakokeet(HakemusDTO hakemus,
                         List<ValintaperusteetDTO> valintaperuste)
                        throws RuntimeException;
}
