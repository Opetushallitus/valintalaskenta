package fi.vm.sade.valintalaskenta.laskenta.service.impl.conversion;

import fi.vm.sade.service.valintaperusteet.model.Arvokonvertteriparametri;
import fi.vm.sade.service.valintaperusteet.model.TekstiRyhma;
import fi.vm.sade.service.valintaperusteet.schema.ArvokonvertteriparametriTyyppi;
import fi.vm.sade.service.valintaperusteet.schema.LokalisoituTekstiTyyppi;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

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
