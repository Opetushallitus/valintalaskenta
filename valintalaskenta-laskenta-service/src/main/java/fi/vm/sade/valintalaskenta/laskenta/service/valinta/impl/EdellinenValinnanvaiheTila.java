package fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl;

import fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila;

/**
 * User: wuoti
 * Date: 4.9.2013
 * Time: 14.42
 */
public class EdellinenValinnanvaiheTila {
    public EdellinenValinnanvaiheTila(JarjestyskriteerituloksenTila tila,
                                      String selite) {
        this.tila = tila;
        this.selite = selite;
    }

    private JarjestyskriteerituloksenTila tila;
    private String selite;

    public JarjestyskriteerituloksenTila getTila() {
        return tila;
    }

    public String getSelite() {
        return selite;
    }
}
