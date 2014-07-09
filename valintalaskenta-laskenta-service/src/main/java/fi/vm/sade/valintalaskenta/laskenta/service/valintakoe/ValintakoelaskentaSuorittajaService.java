package fi.vm.sade.valintalaskenta.laskenta.service.valintakoe;

import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;

import java.util.List;

/**
 * User: wuoti
 * Date: 2.5.2013
 * Time: 9.15
 */
public interface ValintakoelaskentaSuorittajaService {

    void laskeRest(HakemusDTO hakemus,  List<ValintaperusteetDTO> valintaperusteet);
}
