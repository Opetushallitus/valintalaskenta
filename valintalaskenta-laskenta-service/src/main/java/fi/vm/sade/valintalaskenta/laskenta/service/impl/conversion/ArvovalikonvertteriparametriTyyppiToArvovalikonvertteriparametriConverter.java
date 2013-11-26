package fi.vm.sade.valintalaskenta.laskenta.service.impl.conversion;

import fi.vm.sade.service.valintaperusteet.model.Arvovalikonvertteriparametri;
import fi.vm.sade.service.valintaperusteet.schema.ArvovalikonvertteriparametriTyyppi;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
@Component("ArvovalikonvertteriparametriKonvertteri")
public class ArvovalikonvertteriparametriTyyppiToArvovalikonvertteriparametriConverter implements
        Converter<ArvovalikonvertteriparametriTyyppi, Arvovalikonvertteriparametri> {

    public Arvovalikonvertteriparametri convert(ArvovalikonvertteriparametriTyyppi source) {
        Arvovalikonvertteriparametri target = new Arvovalikonvertteriparametri();
        target.setMaxValue(source.getMaksimiarvo());
        target.setMinValue(source.getMinimiarvo());
        target.setPalautaHaettuArvo(source.getPalautaHaettuArvo());
        target.setPaluuarvo(source.getPaluuarvo());
        return target;
    }
}
