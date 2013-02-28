package fi.vm.sade.valintalaskenta.service.impl.conversion;

import java.util.HashMap;
import java.util.Map;

import org.springframework.core.convert.converter.Converter;

import fi.vm.sade.service.hakemus.schema.HakemusTyyppi;
import fi.vm.sade.service.hakemus.schema.HakukohdeTyyppi;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakemus;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
public class HakemusTyyppiToHakemusConverter implements Converter<HakemusTyyppi, Hakemus> {

    public Hakemus convert(HakemusTyyppi source) {
        Map<Integer, String> prioriteettiHakukohde = new HashMap<Integer, String>();
        for (HakukohdeTyyppi hakukohde : source.getHakukohde()) {
            prioriteettiHakukohde.put(hakukohde.getPrioriteetti(), hakukohde.getHakukohdeOid());
        }
        return new Hakemus(prioriteettiHakukohde, HakemusTyyppiUtil.extract(source));
    }

}
