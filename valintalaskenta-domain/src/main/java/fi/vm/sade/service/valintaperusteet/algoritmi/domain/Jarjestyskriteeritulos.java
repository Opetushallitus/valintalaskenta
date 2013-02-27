package fi.vm.sade.service.valintaperusteet.algoritmi.domain;

import com.google.code.morphia.annotations.Embedded;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
@Embedded("Jarjestyskriteeritulos")
public class Jarjestyskriteeritulos {

    private double arvo;
    private int prioriteetti;
    private JarjestyskriteerituloksenTila tila;
    private String kuvaus;
    private String hakemusoid;

    public String getKuvaus() {
        return kuvaus;
    }

    public void setKuvaus(String kuvaus) {
        this.kuvaus = kuvaus;
    }

    public String getHakemusoid() {
        return hakemusoid;
    }

    public void setHakemusoid(String hakemusoid) {
        this.hakemusoid = hakemusoid;
    }

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

    public int getPrioriteetti() {
        return prioriteetti;
    }

    public void setPrioriteetti(int prioriteetti) {
        this.prioriteetti = prioriteetti;
    }

}
