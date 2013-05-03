package fi.vm.sade.valintalaskenta.laskenta.service.impl.conversion;

import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import fi.vm.sade.service.valintaperusteet.model.Arvovalikonvertteriparametri;
import fi.vm.sade.service.valintaperusteet.schema.ArvovalikonvertteriparametriTyyppi;

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
        target.setPalautaHaettuArvo(source.isPalautaHaettuArvo());
        target.setPaluuarvo(source.getPaluuarvo());
        target.setHylkaysperuste(source.isHylkaysperuste());
        return target;
    }
}
