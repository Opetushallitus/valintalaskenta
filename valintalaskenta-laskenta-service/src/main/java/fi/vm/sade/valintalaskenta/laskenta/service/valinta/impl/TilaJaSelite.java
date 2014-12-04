package fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl;

import fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila;

import java.util.HashMap;
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

    public TilaJaSelite(JarjestyskriteerituloksenTila tila,
                        Map<String, String> selite, String teknineSelite) {
        this.tila = tila;
        this.selite = selite;
        this.tekninenSelite = teknineSelite;
    }

    private JarjestyskriteerituloksenTila tila;
    private Map<String, String> selite;
    private String tekninenSelite;

    public JarjestyskriteerituloksenTila getTila() {
        return tila;
    }

    public Map<String, String> getSelite() {

        if(selite == null) {
            this.selite = new HashMap<>();
        }

        return selite;
    }

    public String getTekninenSelite() {
        return tekninenSelite;
    }

    public void setTekninenSelite(String tekninenSelite) {
        this.tekninenSelite = tekninenSelite;
    }
}
