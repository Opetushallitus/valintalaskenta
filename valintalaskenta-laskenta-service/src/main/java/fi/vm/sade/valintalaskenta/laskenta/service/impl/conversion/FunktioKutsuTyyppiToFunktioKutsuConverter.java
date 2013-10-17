package fi.vm.sade.valintalaskenta.laskenta.service.impl.conversion;

import fi.vm.sade.service.valintaperusteet.model.Funktiokutsu;
import fi.vm.sade.service.valintaperusteet.model.Funktionimi;
import fi.vm.sade.service.valintaperusteet.schema.*;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

/**
 * @author Jussi Jartamo
 */
@Component("FunktioKutsuKonvertteri")
public class FunktioKutsuTyyppiToFunktioKutsuConverter implements Converter<FunktiokutsuTyyppi, Funktiokutsu> {

    public Funktiokutsu convert(FunktiokutsuTyyppi source) {
        Funktiokutsu target = new Funktiokutsu();

        FunktioargumenttiTyyppiToFunktioargumenttiConverter fargConverter;
        fargConverter = new FunktioargumenttiTyyppiToFunktioargumenttiConverter();
        for (FunktioargumenttiTyyppi f : source.getFunktioargumentit()) {
            target.getFunktioargumentit().add(fargConverter.convert(f));
        }

        target.setFunktionimi(Funktionimi.valueOf(source.getFunktionimi()));
        target.setId(target.getId());

        ArvokonvertteriparametriTyyppiToArvokonvertteriparametriConverter arvoparamConverter = new ArvokonvertteriparametriTyyppiToArvokonvertteriparametriConverter();

        for (ArvokonvertteriparametriTyyppi k : source.getArvokonvertteriparametrit()) {
            target.getArvokonvertteriparametrit().add(arvoparamConverter.convert(k));
        }

        ArvovalikonvertteriparametriTyyppiToArvovalikonvertteriparametriConverter arvovaliparamConverter = new ArvovalikonvertteriparametriTyyppiToArvovalikonvertteriparametriConverter();
        for (ArvovalikonvertteriparametriTyyppi k : source.getArvovalikonvertteriparametrit()) {
            target.getArvovalikonvertteriparametrit().add(arvovaliparamConverter.convert(k));
        }

        SyoteparametriTyyppiToSyoteparametriConverter sparamConverter = new SyoteparametriTyyppiToSyoteparametriConverter();
        for (SyoteparametriTyyppi s : source.getSyoteparametrit()) {
            target.getSyoteparametrit().add(sparamConverter.convert(s));
        }

        ValintaperusteViiteTyyppiToValintaperusteViiteConverter vConverter = new ValintaperusteViiteTyyppiToValintaperusteViiteConverter();
        for (ValintaperusteviiteTyyppi vp : source.getValintaperusteviite()) {
            target.getValintaperusteviitteet().add(vConverter.convert(vp));
        }

        return target;
    }
}
