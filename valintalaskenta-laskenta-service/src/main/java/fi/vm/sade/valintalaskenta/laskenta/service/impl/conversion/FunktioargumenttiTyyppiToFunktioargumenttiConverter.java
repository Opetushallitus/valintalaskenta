package fi.vm.sade.valintalaskenta.laskenta.service.impl.conversion;

import fi.vm.sade.service.valintaperusteet.model.Funktioargumentti;
import fi.vm.sade.service.valintaperusteet.schema.FunktioargumenttiTyyppi;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

/**
 * @author Jussi Jartamo
 */
@Component("FunktioargumenttiKonvertteri")
public class FunktioargumenttiTyyppiToFunktioargumenttiConverter implements
        Converter<FunktioargumenttiTyyppi, Funktioargumentti> {

    public Funktioargumentti convert(FunktioargumenttiTyyppi source) {
        Funktioargumentti target = new Funktioargumentti();
        if (source.getFunktiokutsu() != null) {
            target.setFunktiokutsuChild(new FunktioKutsuTyyppiToFunktioKutsuConverter().convert(source
                    .getFunktiokutsu()));
        }
        target.setIndeksi(source.getIndeksi());
        return target;
    }
}
