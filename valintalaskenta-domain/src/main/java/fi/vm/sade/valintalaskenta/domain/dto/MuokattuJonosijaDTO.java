package fi.vm.sade.valintalaskenta.domain.dto;

import fi.vm.sade.valintalaskenta.domain.JarjestyskriteerituloksenTila;

import java.math.BigDecimal;

/**
 * Created with IntelliJ IDEA.
 * User: jukais
 * Date: 15.8.2013
 * Time: 7.33
 * To change this template use File | Settings | File Templates.
 */
public class MuokattuJonosijaDTO {
    private JarjestyskriteerituloksenTila tila;
    private BigDecimal arvo;

    public JarjestyskriteerituloksenTila getTila() {
        return tila;
    }

    public void setTila(JarjestyskriteerituloksenTila tila) {
        this.tila = tila;
    }

    public BigDecimal getArvo() {
        return arvo;
    }

    public void setArvo(BigDecimal arvo) {
        this.arvo = arvo;
    }
}
