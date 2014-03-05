package fi.vm.sade.valintalaskenta.laskenta.service.impl.conversion;

import fi.vm.sade.service.valintaperusteet.model.Arvovalikonvertteriparametri;
import fi.vm.sade.service.valintaperusteet.model.TekstiRyhma;
import fi.vm.sade.service.valintaperusteet.schema.ArvovalikonvertteriparametriTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.LokalisoituTekstiTyyppi;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
@Component("ArvovalikonvertteriparametriKonvertteri")
public class ArvovalikonvertteriparametriTyyppiToArvovalikonvertteriparametriConverter implements
        Converter<ArvovalikonvertteriparametriTyyppi, Arvovalikonvertteriparametri> {

    public Arvovalikonvertteriparametri convert(ArvovalikonvertteriparametriTyyppi source) {
        Arvovalikonvertteriparametri target = new Arvovalikonvertteriparametri();
        target.setMaxValue(source.getMaksimiarvo());
        target.setMinValue(source.getMinimiarvo());
        target.setPalautaHaettuArvo(source.getPalautaHaettuArvo());
        target.setPaluuarvo(source.getPaluuarvo());

        target.setHylkaysperuste(source.getHylkaysperuste());

        LokalisoituTekstiTyyppiToLokalisoituTekstiConverter converter = new LokalisoituTekstiTyyppiToLokalisoituTekstiConverter();
        TekstiRyhma ryhma = new TekstiRyhma();
        if(source.getKuvaukset() != null && source.getKuvaukset().getTekstit() != null) {
            for (LokalisoituTekstiTyyppi k : source.getKuvaukset().getTekstit()) {
                ryhma.getTekstit().add(converter.convert(k));
            }
        }
        target.setKuvaukset(ryhma);

        return target;
    }
}
