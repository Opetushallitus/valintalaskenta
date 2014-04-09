package fi.vm.sade.valintalaskenta.domain.valinta;

import com.google.code.morphia.annotations.Embedded;

/**
 * User: wuoti
 * Date: 17.9.2013
 * Time: 14.33

 */
@Embedded
public class FunktioTulos {
    private String tunniste;
    private String arvo;
    private String nimiFi;
    private String nimiSv;
    private String nimiEn;

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

    public String getNimiFi() {
        return nimiFi;
    }

    public void setNimiFi(String nimiFi) {
        this.nimiFi = nimiFi;
    }

    public String getNimiSv() {
        return nimiSv;
    }

    public void setNimiSv(String nimiSv) {
        this.nimiSv = nimiSv;
    }

    public String getNimiEn() {
        return nimiEn;
    }

    public void setNimiEn(String nimiEn) {
        this.nimiEn = nimiEn;
    }
}
