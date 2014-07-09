package fi.vm.sade.valintalaskenta.laskenta.service.valinta;

import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;

import java.util.List;

/**
 * 
 * @author Jussi Jartamo
 * 
 *         Konvertoi WSDL tyypit laskentaa varten
 * 
 */
public interface ValintalaskentaSuorittajaService {

    /**
     * 
     * @param hakemukset
     * @param valintaperusteet
     */

    void suoritaLaskentaRest(List<HakemusDTO> hakemukset, List<ValintaperusteetDTO> valintaperusteet);

}
