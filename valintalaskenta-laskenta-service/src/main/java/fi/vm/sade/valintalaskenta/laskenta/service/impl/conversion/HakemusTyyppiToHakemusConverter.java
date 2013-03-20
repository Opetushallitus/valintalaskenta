package fi.vm.sade.valintalaskenta.laskenta.service.impl.conversion;

import java.util.HashMap;
import java.util.Map;

import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import fi.vm.sade.service.hakemus.schema.HakemusTyyppi;
import fi.vm.sade.service.hakemus.schema.HakukohdeTyyppi;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakemus;
import fi.vm.sade.valintalaskenta.laskenta.HakemusTyyppiUtil;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
@Component("HakemusKonvertteri")
public class HakemusTyyppiToHakemusConverter implements Converter<HakemusTyyppi, Hakemus> {

    public Hakemus convert(HakemusTyyppi source) {
        Map<Integer, String> prioriteettiHakukohde = new HashMap<Integer, String>();
        for (HakukohdeTyyppi hakukohde : source.getHakutoive()) {
            prioriteettiHakukohde.put(hakukohde.getPrioriteetti(), hakukohde.getHakukohdeOid());
        }
        return new Hakemus(source.getHakemusOid(), prioriteettiHakukohde, HakemusTyyppiUtil.extract(source));
    }

}
