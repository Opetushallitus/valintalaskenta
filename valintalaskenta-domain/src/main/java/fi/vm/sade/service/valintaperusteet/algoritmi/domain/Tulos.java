package fi.vm.sade.service.valintaperusteet.algoritmi.domain;

import java.util.HashSet;
import java.util.Set;

import com.google.code.morphia.annotations.Embedded;

/**
 * 
 * @author Jussi Jartamo
 *
 */
@Embedded
public class Tulos {

    private TuloksenTila tila;
    private double arvo;

    @Embedded
    private Jono jono;

    @Embedded
    private Set<Jarjestyskriteeritulos> jarjestyskriteeritulokset = new HashSet<Jarjestyskriteeritulos>();

    public Set<Jarjestyskriteeritulos> getJarjestyskriteeritulokset() {
        return jarjestyskriteeritulokset;
    }

    public void setJarjestyskriteeritulokset(Set<Jarjestyskriteeritulos> jarjestyskriteeritulokset) {
        this.jarjestyskriteeritulokset = jarjestyskriteeritulokset;
    }

    public Jono getJono() {
        return jono;
    }

    public void setJono(Jono jono) {
        this.jono = jono;
    }
    public double getArvo() {
        return arvo;
    }
    public void setArvo(double arvo) {
        this.arvo = arvo;
    }
    public TuloksenTila getTila() {
        return tila;
    }

    public void setTila(TuloksenTila tila) {
        this.tila = tila;
    }
}
