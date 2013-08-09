package fi.vm.sade.valintalaskenta.laskenta.service.impl.conversion;

import fi.vm.sade.service.valintaperusteet.model.ValintaperusteViite;
import fi.vm.sade.service.valintaperusteet.model.Valintaperustelahde;
import fi.vm.sade.service.valintaperusteet.schema.ValintaperusteviiteTyyppi;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

/**
 * @author Jussi Jartamo
 */
@Component("ValintaperusteViiteKonvertteri")
public class ValintaperusteViiteTyyppiToValintaperusteViiteConverter implements
        Converter<ValintaperusteviiteTyyppi, ValintaperusteViite> {

    public ValintaperusteViite convert(ValintaperusteviiteTyyppi source) {
        ValintaperusteViite target = new ValintaperusteViite();
        target.setTunniste(source.getTunniste());
        target.setLahde(Valintaperustelahde.valueOf(source.getLahde().name()));
        target.setOnPakollinen(source.isOnPakollinen());
        return target;
    }

}
