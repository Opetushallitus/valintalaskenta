package fi.vm.sade.service.valintalaskenta.service.impl.conversion;

import fi.vm.sade.service.valintaperusteet.model.Arvokonverteriparametri;
import fi.vm.sade.service.valintaperusteet.schema.ArvokonverteriparametriTyyppi;
import org.springframework.core.convert.converter.Converter;

/**
 * 
 * @author Jussi Jartamo
 *
 */
public class ArvokonverteriparametriTyyppiToArvokonverteriparametriConverter implements
        Converter<ArvokonverteriparametriTyyppi, Arvokonverteriparametri> {

    public Arvokonverteriparametri convert(ArvokonverteriparametriTyyppi source) {
        Arvokonverteriparametri target = new Arvokonverteriparametri();
        target.setArvo(source.getArvo());
        target.setPaluuarvo(source.getPaluuarvo());
        target.setHylkaysperuste(source.isHylkaysperuste());
        return target;
    }
}
