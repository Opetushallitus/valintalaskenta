package fi.vm.sade.valintalaskenta.laskenta.service;

import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetHakijaryhmaDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;

import java.util.List;
import java.util.Map;

/**
 * Created by jukais on 24.3.2014.
 */
public interface ValintalaskentaService {

    String laske(List<HakemusDTO> hakemus,
                 List<ValintaperusteetDTO> valintaperuste,
                 List<ValintaperusteetHakijaryhmaDTO> hakijaryhmat,
                 String hakukohdeOid)
                throws RuntimeException;


    String valintakokeet(HakemusDTO hakemus,
                         List<ValintaperusteetDTO> valintaperuste)
                        throws RuntimeException;

    String laskeKaikki(List<HakemusDTO> hakemus,
                 List<ValintaperusteetDTO> valintaperuste,
                 List<ValintaperusteetHakijaryhmaDTO> hakijaryhmat,
                 String hakukohdeOid)
            throws RuntimeException;

    void applyValisijoittelu(Map<String, List<String>> valisijoiteltavatJonot, Map<String, fi.vm.sade.sijoittelu.tulos.dto.HakemusDTO> hakemusHashMap);
}
