package fi.vm.sade.valintalaskenta.domain.dto.valintakoe;

import fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen;

/**
 * User: wuoti
 * Date: 29.8.2013
 * Time: 8.32
 */
public class OsallistuminenTulosDTO {
    private Osallistuminen osallistuminen;
    private String kuvaus;
    private String laskentaTila;
    private Boolean laskentaTulos;

    public Osallistuminen getOsallistuminen() {
        return osallistuminen;
    }

    public void setOsallistuminen(Osallistuminen osallistuminen) {
        this.osallistuminen = osallistuminen;
    }

    public String getKuvaus() {
        return kuvaus;
    }

    public void setKuvaus(String kuvaus) {
        this.kuvaus = kuvaus;
    }

    public String getLaskentaTila() {
        return laskentaTila;
    }

    public void setLaskentaTila(String laskentaTila) {
        this.laskentaTila = laskentaTila;
    }

    public Boolean getLaskentaTulos() {
        return laskentaTulos;
    }

    public void setLaskentaTulos(Boolean laskentaTulos) {
        this.laskentaTulos = laskentaTulos;
    }
}
