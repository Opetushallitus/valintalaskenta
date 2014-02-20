package fi.vm.sade.valintalaskenta.laskenta.service.impl.conversion;

import fi.vm.sade.service.valintaperusteet.dto.model.Kieli;
import fi.vm.sade.service.valintaperusteet.model.LokalisoituTeksti;
import fi.vm.sade.service.valintaperusteet.schema.LokalisoituTekstiTyyppi;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

/**
 *
 * @author Jussi Jartamo
 *
 */
@Component("LokalisoituTekstiKonvertteri")
public class LokalisoituTekstiTyyppiToLokalisoituTekstiConverter implements Converter<LokalisoituTekstiTyyppi, LokalisoituTeksti> {

    public LokalisoituTeksti convert(LokalisoituTekstiTyyppi source) {
        LokalisoituTeksti target = new LokalisoituTeksti();
        target.setKieli(Kieli.valueOf(source.getKieli().name()));
        target.setTeksti(source.getTeksti());
        return target;
    }

}
