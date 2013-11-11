package fi.vm.sade.valintalaskenta.laskenta;

import fi.vm.sade.service.hakemus.schema.AvainArvoTyyppi;
import fi.vm.sade.service.hakemus.schema.HakemusTyyppi;

import java.util.HashMap;
import java.util.Map;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
public class HakemusTyyppiUtil {

    public static Map<String, String> extract(HakemusTyyppi source) {
        Map<String, String> target = new HashMap<String, String>();
        for (AvainArvoTyyppi a : source.getAvainArvo()) {
            target.put(a.getAvain(), a.getArvo());
        }
        return target;
    }
}
