package fi.vm.sade.service.valintalaskenta.service.impl.conversion;

import fi.vm.sade.service.valintaperusteet.model.Arvovalikonverteriparametri;
import fi.vm.sade.service.valintaperusteet.schema.ArvovalikonverteriparametriTyyppi;
import org.springframework.core.convert.converter.Converter;

/**
 * 
 * @author Jussi Jartamo
 *
 */
public class ArvovalikonverteriparametriTyyppiToArvovalikonverteriparametriConverter implements
        Converter<ArvovalikonverteriparametriTyyppi, Arvovalikonverteriparametri> {

    public Arvovalikonverteriparametri convert(ArvovalikonverteriparametriTyyppi source) {
        Arvovalikonverteriparametri target = new Arvovalikonverteriparametri();
        target.setMaxValue(source.getMaksimiarvo());
        target.setMinValue(source.getMinimiarvo());
        target.setPalautaHaettuArvo(source.isPalautaHaettuArvo());
        target.setPaluuarvo(source.getPaluuarvo());
        target.setHylkaysperuste(source.isHylkaysperuste());
        return target;
    }
}
