package fi.vm.sade.valintalaskenta.laskenta.service.impl.conversion;

import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakemus;
import fi.vm.sade.valintalaskenta.domain.dto.AvainArvoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakukohdeDTO;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

/**
 *
 * @author Jussi Jartamo
 *
 */
@Component("HakemusDTOKonvertteri")
public class HakemusDTOToHakemusConverter implements Converter<HakemusDTO, Hakemus> {

    public Hakemus convert(HakemusDTO dto) {
        Map<Integer, String> prioriteettiHakukohde = dto.getHakukohteet().stream().collect(Collectors.toMap(HakukohdeDTO::getPrioriteetti, HakukohdeDTO::getOid));
        Map<String, String> target = dto.getAvaimet().stream().collect(Collectors.toMap(
                AvainArvoDTO::getAvain, AvainArvoDTO::getArvo,
                (s,a) -> s + ", " + a));
        //TODO suoritukset
        return new Hakemus(dto.getHakemusoid(), prioriteettiHakukohde, target, new HashMap<>());
    }

}
