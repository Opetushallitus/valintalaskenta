package fi.vm.sade.valintalaskenta.laskenta.service.impl.conversion;

import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakemus;
import fi.vm.sade.valintalaskenta.domain.dto.AvainArvoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakukohdeDTO;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author Jussi Jartamo
 *
 */
@Component("HakemusDTOKonvertteri")
public class HakemusDTOToHakemusConverter implements Converter<HakemusDTO, Hakemus> {

    public Hakemus convert(HakemusDTO dto) {
        Map<Integer, String> prioriteettiHakukohde = new HashMap<Integer, String>();
        if(dto.getHakukohteet() != null) {
            for (HakukohdeDTO hakukohde : dto.getHakukohteet()) {
                prioriteettiHakukohde.put(hakukohde.getPrioriteetti(), hakukohde.getOid());
            }
        }
        Map<String, String> target = new HashMap<String, String>();
        if(dto.getAvaimet() != null) {
            for (AvainArvoDTO a : dto.getAvaimet()) {
                target.put(a.getAvain(), a.getArvo());
            }
        }

        return new Hakemus(dto.getHakemusoid(), prioriteettiHakukohde, target);
    }

}
