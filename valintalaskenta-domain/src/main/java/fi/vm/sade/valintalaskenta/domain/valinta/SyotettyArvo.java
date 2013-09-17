package fi.vm.sade.valintalaskenta.domain.valinta;

import com.google.code.morphia.annotations.Embedded;

/**
 * User: wuoti
 * Date: 17.9.2013
 * Time: 14.33
 */
@Embedded
public class SyotettyArvo {
    private String tunniste;
    private String arvo;
    private String laskennallinenArvo;
    private String osallistumien;

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

    public String getLaskennallinenArvo() {
        return laskennallinenArvo;
    }

    public void setLaskennallinenArvo(String laskennallinenArvo) {
        this.laskennallinenArvo = laskennallinenArvo;
    }

    public String getOsallistumien() {
        return osallistumien;
    }

    public void setOsallistumien(String osallistumien) {
        this.osallistumien = osallistumien;
    }
}
