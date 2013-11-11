package fi.vm.sade.valintalaskenta.laskenta.service.impl.conversion;

import fi.vm.sade.service.valintaperusteet.model.Syoteparametri;
import fi.vm.sade.service.valintaperusteet.schema.SyoteparametriTyyppi;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
@Component("SyoteparametriKonvertteri")
public class SyoteparametriTyyppiToSyoteparametriConverter implements Converter<SyoteparametriTyyppi, Syoteparametri> {

    public Syoteparametri convert(SyoteparametriTyyppi source) {
        Syoteparametri target = new Syoteparametri();
        target.setArvo(source.getArvo());
        target.setAvain(source.getAvain());
        return target;
    }
}
