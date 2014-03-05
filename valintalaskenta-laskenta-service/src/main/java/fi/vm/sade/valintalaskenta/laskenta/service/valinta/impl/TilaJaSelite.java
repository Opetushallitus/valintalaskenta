package fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl;

import fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila;

import java.util.Map;

/**
 * User: wuoti
 * Date: 4.9.2013
 * Time: 14.42
 */
public class TilaJaSelite {
    public TilaJaSelite(JarjestyskriteerituloksenTila tila,
                        Map<String, String> selite) {
        this.tila = tila;
        this.selite = selite;
    }

    private JarjestyskriteerituloksenTila tila;
    private Map<String, String> selite;

    public JarjestyskriteerituloksenTila getTila() {
        return tila;
    }

    public Map<String, String> getSelite() {
        return selite;
    }
}
