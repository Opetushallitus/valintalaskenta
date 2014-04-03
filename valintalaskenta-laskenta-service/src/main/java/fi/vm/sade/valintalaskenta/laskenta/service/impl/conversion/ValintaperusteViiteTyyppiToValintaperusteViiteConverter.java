package fi.vm.sade.valintalaskenta.laskenta.service.impl.conversion;

import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import fi.vm.sade.service.valintaperusteet.model.ValintaperusteViite;
import fi.vm.sade.service.valintaperusteet.dto.model.Valintaperustelahde;
import fi.vm.sade.service.valintaperusteet.schema.ValintaperusteviiteTyyppi;

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
        target.setOnPakollinen(Boolean.TRUE.equals(source.isOnPakollinen()));
        target.setIndeksi(source.getIndeksi());
        target.setEpasuoraViittaus(Boolean.TRUE.equals(source.isEpasuoraViittaus()));
        target.setKuvaus(source.getKuvaus());
        return target;
    }

}
