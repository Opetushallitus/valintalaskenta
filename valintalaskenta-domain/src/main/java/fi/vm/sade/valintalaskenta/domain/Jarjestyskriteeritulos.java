package fi.vm.sade.valintalaskenta.domain;

import com.google.code.morphia.annotations.Embedded;
import org.codehaus.jackson.map.annotate.JsonView;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
@Embedded("Jarjestyskriteeritulos")
public class Jarjestyskriteeritulos {

    @JsonView(JsonViews.Basic.class)
    private double arvo;

    @JsonView(JsonViews.Basic.class)
    private JarjestyskriteerituloksenTila tila;

    @JsonView(JsonViews.Basic.class)
    private String kuvaus;

    public double getArvo() {
        return arvo;
    }

    public void setArvo(double arvo) {
        this.arvo = arvo;
    }

    public JarjestyskriteerituloksenTila getTila() {
        return tila;
    }

    public void setTila(JarjestyskriteerituloksenTila tila) {
        this.tila = tila;
    }

    public String getKuvaus() {
        return kuvaus;
    }

    public void setKuvaus(String kuvaus) {
        this.kuvaus = kuvaus;
    }
}
