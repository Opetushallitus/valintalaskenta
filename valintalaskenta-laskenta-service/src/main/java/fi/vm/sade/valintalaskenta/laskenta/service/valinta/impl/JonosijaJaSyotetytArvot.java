package fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl;

import fi.vm.sade.service.valintaperusteet.laskenta.api.SyotettyArvo;
import fi.vm.sade.valintalaskenta.domain.valinta.Jonosija;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * User: wuoti
 * Date: 17.9.2013
 * Time: 14.26
 */
public class JonosijaJaSyotetytArvot {
    public JonosijaJaSyotetytArvot(Jonosija jonosija, Map<String, SyotettyArvo> syotetytArvot) {
        this.jonosija = jonosija;
        this.syotetytArvot = syotetytArvot;
    }

    public JonosijaJaSyotetytArvot(Jonosija jonosija) {
        this(jonosija, new HashMap<String, SyotettyArvo>());
    }

    private Jonosija jonosija;
    private Map<String, SyotettyArvo> syotetytArvot = new HashMap<String, SyotettyArvo>();

    public Jonosija getJonosija() {
        return jonosija;
    }

    public Map<String, SyotettyArvo> getSyotetytArvot() {
        return Collections.unmodifiableMap(syotetytArvot);
    }

    public void lisaaSyotetytArvot(Map<String, SyotettyArvo> syotetytArvot) {
        this.syotetytArvot.putAll(syotetytArvot);
    }
}
