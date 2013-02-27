package fi.vm.sade.valintalaskenta.service.impl.conversion;

import org.springframework.core.convert.converter.Converter;

import fi.vm.sade.service.valintaperusteet.model.Syoteparametri;
import fi.vm.sade.service.valintaperusteet.schema.SyoteparametriTyyppi;

/**
 * 
 * @author Jussi Jartamo
 *
 */
public class SyoteparametriTyyppiToSyoteparametriConverter implements Converter<SyoteparametriTyyppi, Syoteparametri> {

    public Syoteparametri convert(SyoteparametriTyyppi source) {
        Syoteparametri target = new Syoteparametri();
        target.setArvo(source.getArvo());
        target.setAvain(source.getAvain());
        return target;
    }
}
