package fi.vm.sade.valintalaskenta.service.impl.conversion;

import fi.vm.sade.service.valintaperusteet.model.Funktioargumentti;
import fi.vm.sade.service.valintaperusteet.schema.FunktioargumenttiTyyppi;
import org.springframework.core.convert.converter.Converter;

/**
 * @author Jussi Jartamo
 */
public class FunktioargumenttiTyyppiToFunktioargumenttiConverter implements
        Converter<FunktioargumenttiTyyppi, Funktioargumentti> {

    public Funktioargumentti convert(FunktioargumenttiTyyppi source) {
        Funktioargumentti target = new Funktioargumentti();
        if (source.getFunktiokutsu() != null) {
            target.setFunktiokutsuChild(new FunktioKutsuTyyppiToFunktioKutsuConverter().convert(source.getFunktiokutsu()));
        }
        target.setIndeksi(source.getIndeksi());
        return target;
    }
}
