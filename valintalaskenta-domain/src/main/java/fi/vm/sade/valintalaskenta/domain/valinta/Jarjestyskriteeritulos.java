package fi.vm.sade.valintalaskenta.domain.valinta;

import com.google.code.morphia.annotations.Converters;
import com.google.code.morphia.annotations.Embedded;
import fi.vm.sade.valintalaskenta.domain.JsonViews;
import fi.vm.sade.valintalaskenta.domain.converter.BigDecimalConverter;
import org.bson.types.ObjectId;
import org.codehaus.jackson.map.annotate.JsonView;

import java.math.BigDecimal;

/**
 * User: wuoti
 * Date: 4.9.2013
 * Time: 10.30
 */
@Embedded
@Converters(BigDecimalConverter.class)
public class Jarjestyskriteeritulos {
    @JsonView(JsonViews.Basic.class)
    private int prioriteetti;

    @JsonView(JsonViews.Basic.class)
    private BigDecimal arvo;

    @JsonView(JsonViews.Basic.class)
    private JarjestyskriteerituloksenTila tila;

    @JsonView(JsonViews.Basic.class)
    private String kuvaus;

    private ObjectId historia;

    public int getPrioriteetti() {
        return prioriteetti;
    }

    public void setPrioriteetti(int prioriteetti) {
        this.prioriteetti = prioriteetti;
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

    public ObjectId getHistoria() {
        return historia;
    }

    public void setHistoria(ObjectId historia) {
        this.historia = historia;
    }
}
