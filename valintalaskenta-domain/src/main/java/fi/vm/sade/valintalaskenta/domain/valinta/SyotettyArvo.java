package fi.vm.sade.valintalaskenta.domain.valinta;

import org.mongodb.morphia.annotations.Embedded;

@Embedded
public class SyotettyArvo {
    private String tunniste;
    private String arvo;
    private String laskennallinenArvo;
    private String osallistuminen;

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

    public String getOsallistuminen() {
        return osallistuminen;
    }

    public void setOsallistuminen(String osallistuminen) {
        this.osallistuminen = osallistuminen;
    }
}
