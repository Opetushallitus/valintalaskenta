package fi.vm.sade.valintalaskenta.laskenta.service;

import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetHakijaryhmaDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;

import java.util.List;
import java.util.Map;

public interface ValintalaskentaService {
    String laske(List<HakemusDTO> hakemus,
                 List<ValintaperusteetDTO> valintaperuste,
                 List<ValintaperusteetHakijaryhmaDTO> hakijaryhmat,
                 String hakukohdeOid,
                 String uuid)
                throws RuntimeException;

    String valintakokeet(HakemusDTO hakemus,
                         List<ValintaperusteetDTO> valintaperuste,
                         String uuid)
                        throws RuntimeException;

    String laskeKaikki(List<HakemusDTO> hakemus,
                 List<ValintaperusteetDTO> valintaperuste,
                 List<ValintaperusteetHakijaryhmaDTO> hakijaryhmat,
                 String hakukohdeOid,
                 String uuid)
            throws RuntimeException;

    void applyValisijoittelu(Map<String, List<String>> valisijoiteltavatJonot, Map<String, fi.vm.sade.sijoittelu.tulos.dto.HakemusDTO> hakemusHashMap);

    void applyErillissijoittelu(Map<String, List<String>> jonot, Long ajo);
}
