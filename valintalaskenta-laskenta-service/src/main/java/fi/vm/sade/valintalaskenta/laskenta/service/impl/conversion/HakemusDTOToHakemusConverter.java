package fi.vm.sade.valintalaskenta.laskenta.service.impl.conversion;

import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakemus;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakutoive;
import fi.vm.sade.valintalaskenta.domain.dto.AvainArvoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.AvainMetatiedotDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakukohdeDTO;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

@Component("HakemusDTOKonvertteri")
public class HakemusDTOToHakemusConverter implements Converter<HakemusDTO, Hakemus> {

    private static Function<HakukohdeDTO, Hakutoive> getHakutoive = hakukohdeDTO -> new Hakutoive (hakukohdeDTO.getOid(), hakukohdeDTO.getHakukohdeRyhmatOids());

    private static Function<HakukohdeDTO, Integer> getPrioriteetti = HakukohdeDTO::getPrioriteetti;

    public Hakemus convert(HakemusDTO dto) {
        Map<Integer, Hakutoive> prioriteettiHakukohde = dto.getHakukohteet().stream().collect(Collectors.toMap(getPrioriteetti, getHakutoive));
        Map<String, String> target = dto.getAvaimet().stream().collect(Collectors.toMap(
                AvainArvoDTO::getAvain, AvainArvoDTO::getArvo,
                (s,a) -> s + ", " + a));
        Map<String, List<Map<String,String>>> metatiedot = dto.getAvainMetatiedotDTO().stream().collect(Collectors.toMap(
                AvainMetatiedotDTO::getAvain, AvainMetatiedotDTO::getMetatiedot,
                (s,a) -> {
                    s.addAll(a);
                    return s;
                }));
        return new Hakemus(dto.getHakemusoid(), prioriteettiHakukohde, target, metatiedot);
    }

}
