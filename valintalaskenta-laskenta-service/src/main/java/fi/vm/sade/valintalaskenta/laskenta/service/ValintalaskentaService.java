package fi.vm.sade.valintalaskenta.laskenta.service;

import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetHakijaryhmaDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;

import java.util.List;

/**
 * Created by jukais on 24.3.2014.
 */
public interface ValintalaskentaService {

    String laske(List<HakemusDTO> hakemus,
                 List<ValintaperusteetDTO> valintaperuste,
                 List<ValintaperusteetHakijaryhmaDTO> hakijaryhmat)
                throws RuntimeException;


    String valintakokeet(HakemusDTO hakemus,
                         List<ValintaperusteetDTO> valintaperuste)
                        throws RuntimeException;

    String laskeKaikki(List<HakemusDTO> hakemus,
                 List<ValintaperusteetDTO> valintaperuste,
                 List<ValintaperusteetHakijaryhmaDTO> hakijaryhmat)
            throws RuntimeException;
}
