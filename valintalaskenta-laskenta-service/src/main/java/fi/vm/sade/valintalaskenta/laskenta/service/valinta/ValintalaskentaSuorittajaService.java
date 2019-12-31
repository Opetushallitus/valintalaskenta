package fi.vm.sade.valintalaskenta.laskenta.service.valinta;

import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetHakijaryhmaDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;

import java.util.List;

public interface ValintalaskentaSuorittajaService {

    void suoritaLaskenta(List<HakemusDTO> hakemukset,
                         List<ValintaperusteetDTO> valintaperusteet,
                         List<ValintaperusteetHakijaryhmaDTO> hakijaryhmat,
                         String hakukohdeOid,
                         String uuid, boolean korkeakouluhaku);
}
