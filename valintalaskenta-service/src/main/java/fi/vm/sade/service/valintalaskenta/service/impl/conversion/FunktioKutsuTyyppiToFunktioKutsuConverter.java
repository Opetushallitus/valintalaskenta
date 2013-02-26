package fi.vm.sade.service.valintalaskenta.service.impl.conversion;

import fi.vm.sade.service.valintaperusteet.model.Funktiokutsu;
import fi.vm.sade.service.valintaperusteet.schema.*;
import org.springframework.core.convert.converter.Converter;

/**
 * 
 * @author Jussi Jartamo
 *
 */
public class FunktioKutsuTyyppiToFunktioKutsuConverter implements Converter<FunktiokutsuTyyppi, Funktiokutsu> {

    public Funktiokutsu convert(FunktiokutsuTyyppi source) {
        Funktiokutsu target = new Funktiokutsu();

        FunktioargumenttiTyyppiToFunktioargumenttiConverter fargConverter;
        fargConverter = new FunktioargumenttiTyyppiToFunktioargumenttiConverter();
        for (FunktioargumenttiTyyppi f : source.getFunktioargumentit()) {
            target.getFunktioargumentit().add(fargConverter.convert(f));
        }
        target.setFunktionimi(new FunktionimiTyyppiToFunktionimiConverter().convert(source.getFunktionimi()));
        target.setId(target.getId());

        ArvokonverteriparametriTyyppiToArvokonverteriparametriConverter arvoparamConverter =
                new ArvokonverteriparametriTyyppiToArvokonverteriparametriConverter();

        for (ArvokonverteriparametriTyyppi k : source.getArvokonverteriparametrit()) {
            target.getArvokonverteriparametrit().add(arvoparamConverter.convert(k));
        }

        ArvovalikonverteriparametriTyyppiToArvovalikonverteriparametriConverter arvovaliparamConverter =
                new ArvovalikonverteriparametriTyyppiToArvovalikonverteriparametriConverter();
        for(ArvovalikonverteriparametriTyyppi k : source.getArvovalikonverteriparametrit()) {
            target.getArvovalikonverteriparametrit().add(arvovaliparamConverter.convert(k));
        }

        SyoteparametriTyyppiToSyoteparametriConverter sparamConverter;
        sparamConverter = new SyoteparametriTyyppiToSyoteparametriConverter();
        for (SyoteparametriTyyppi s : source.getSyoteparametrit()) {
            target.getSyoteparametrit().add(sparamConverter.convert(s));
        }
        ValintaperusteViiteTyyppiToValintaperusteViiteConverter vConverter;
        vConverter = new ValintaperusteViiteTyyppiToValintaperusteViiteConverter();
        for(ValintaperusteviiteTyyppi t : source.getValintaperusteviite()) {
            target.getValintaperusteet().add(vConverter.convert(t));
        }
        return target;
    }
}
