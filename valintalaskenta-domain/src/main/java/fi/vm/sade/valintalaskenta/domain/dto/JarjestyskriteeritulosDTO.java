package fi.vm.sade.valintalaskenta.domain.dto;

import com.google.code.morphia.annotations.Converters;
import fi.vm.sade.valintalaskenta.domain.JsonViews;
import fi.vm.sade.valintalaskenta.domain.converter.BigDecimalConverter;
import fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila;
import org.codehaus.jackson.map.annotate.JsonView;

import java.math.BigDecimal;

/**
 * @author Jussi Jartamo
 */
@Converters(BigDecimalConverter.class)
public class JarjestyskriteeritulosDTO implements Comparable<JarjestyskriteeritulosDTO> {

    @JsonView(JsonViews.Basic.class)
    private BigDecimal arvo;

    @JsonView(JsonViews.Basic.class)
    private JarjestyskriteerituloksenTila tila;

    @JsonView(JsonViews.Basic.class)
    private String kuvaus;

    @JsonView(JsonViews.Basic.class)
    private int prioriteetti;

    @Override
    public int compareTo(JarjestyskriteeritulosDTO o) {
        return new Integer(prioriteetti).compareTo(o.getPrioriteetti());
    }

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

    public int getPrioriteetti() {
        return prioriteetti;
    }

    public void setPrioriteetti(int prioriteetti) {
        this.prioriteetti = prioriteetti;
    }
}
