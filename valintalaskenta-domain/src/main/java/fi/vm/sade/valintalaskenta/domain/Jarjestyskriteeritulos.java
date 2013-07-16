package fi.vm.sade.valintalaskenta.domain;

import java.math.BigDecimal;

import org.codehaus.jackson.map.annotate.JsonView;

import com.google.code.morphia.annotations.Converters;
import com.google.code.morphia.annotations.Embedded;

import fi.vm.sade.valintalaskenta.domain.converter.BigDecimalConverter;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
@Embedded("Jarjestyskriteeritulos")
@Converters(BigDecimalConverter.class)
public class Jarjestyskriteeritulos {

    @JsonView(JsonViews.Basic.class)
    private BigDecimal arvo;

    @JsonView(JsonViews.Basic.class)
    private JarjestyskriteerituloksenTila tila;

    @JsonView(JsonViews.Basic.class)
    private String kuvaus;

    public BigDecimal getArvo() {
        return arvo;
    }

    public void setArvo(BigDecimal arvo) {
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
