package fi.vm.sade.service.valintalaskenta.service.impl.conversion;

import java.util.HashMap;
import java.util.Map;

import org.springframework.core.convert.converter.Converter;

import fi.vm.sade.service.hakemus.schema.AvainArvoTyyppi;
import fi.vm.sade.service.hakemus.schema.HakemusTyyppi;

public class HakemusTyyppiToMapConverter implements Converter<HakemusTyyppi, Map<String,String>> {

    public Map<String, String> convert(HakemusTyyppi source) {
        Map<String, String> target = new HashMap<String, String>();
        for(AvainArvoTyyppi a : source.getAvainArvo()) {
            target.put(a.getAvain(), a.getArvo());
        }
        return target;
    }
}
