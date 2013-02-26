package fi.vm.sade.service.valintalaskenta.service.impl.conversion;

import org.springframework.core.convert.converter.Converter;

import fi.vm.sade.service.valintaperusteet.model.Funktioargumentti;
import fi.vm.sade.service.valintaperusteet.schema.FunktioargumenttiTyyppi;

/**
 * 
 * @author Jussi Jartamo
 *
 */
public class FunktioargumenttiTyyppiToFunktioargumenttiConverter implements
        Converter<FunktioargumenttiTyyppi, Funktioargumentti> {

    public Funktioargumentti convert(FunktioargumenttiTyyppi source) {
        Funktioargumentti target = new Funktioargumentti();
        target.setChild(new FunktioKutsuTyyppiToFunktioKutsuConverter().convert(source.getFunktiokutsu()));
        target.setIndeksi(source.getIndeksi());
        return target;
    }
}
