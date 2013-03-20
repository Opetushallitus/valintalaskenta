package fi.vm.sade.valintalaskenta.laskenta.service.impl.conversion;

import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import fi.vm.sade.service.valintaperusteet.model.ValintaperusteViite;
import fi.vm.sade.service.valintaperusteet.schema.ValintaperusteviiteTyyppi;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
@Component("ValintaperusteViiteKonvertteri")
public class ValintaperusteViiteTyyppiToValintaperusteViiteConverter implements
        Converter<ValintaperusteviiteTyyppi, ValintaperusteViite> {

    public ValintaperusteViite convert(ValintaperusteviiteTyyppi source) {
        ValintaperusteViite target = new ValintaperusteViite();
        target.setTunniste(source.getTunniste());
        target.setOnPakollinen(source.isOnPakollinen());
        return target;
    }

}
