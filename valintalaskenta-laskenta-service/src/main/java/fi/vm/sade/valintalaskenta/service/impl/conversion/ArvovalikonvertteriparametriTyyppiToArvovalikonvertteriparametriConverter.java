package fi.vm.sade.valintalaskenta.service.impl.conversion;

import fi.vm.sade.service.valintaperusteet.model.Arvovalikonvertteriparametri;
import fi.vm.sade.service.valintaperusteet.schema.ArvovalikonvertteriparametriTyyppi;
import org.springframework.core.convert.converter.Converter;

/**
 * 
 * @author Jussi Jartamo
 *
 */
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
