package fi.vm.sade.valintalaskenta.domain.valinta;

import com.google.code.morphia.annotations.Embedded;

/**
 * User: wuoti
 * Date: 17.9.2013
 * Time: 14.33
 */
@Embedded
public class Tulos {
    private String tunniste;
    private String arvo;

    public String getTunniste() {
        return tunniste;
    }

    public void setTunniste(String tunniste) {
        this.tunniste = tunniste;
    }

    public String getArvo() {
        return arvo;
    }

    public void setArvo(String arvo) {
        this.arvo = arvo;
    }
}
