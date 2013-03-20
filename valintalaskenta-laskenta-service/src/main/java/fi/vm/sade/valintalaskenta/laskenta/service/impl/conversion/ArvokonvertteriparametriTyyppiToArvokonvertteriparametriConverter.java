package fi.vm.sade.valintalaskenta.laskenta.service.impl.conversion;

import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import fi.vm.sade.service.valintaperusteet.model.Arvokonvertteriparametri;
import fi.vm.sade.service.valintaperusteet.schema.ArvokonvertteriparametriTyyppi;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
@Component("ArvokonvertteriparametriKonvertteri")
public class ArvokonvertteriparametriTyyppiToArvokonvertteriparametriConverter implements
        Converter<ArvokonvertteriparametriTyyppi, Arvokonvertteriparametri> {

    public Arvokonvertteriparametri convert(ArvokonvertteriparametriTyyppi source) {
        Arvokonvertteriparametri target = new Arvokonvertteriparametri();
        target.setArvo(source.getArvo());
        target.setPaluuarvo(source.getPaluuarvo());
        target.setHylkaysperuste(source.isHylkaysperuste());
        return target;
    }
}