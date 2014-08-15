package fi.vm.sade.valintalaskenta.domain.valintakoe;

import org.mongodb.morphia.annotations.Embedded;
import fi.vm.sade.valintalaskenta.domain.JsonViews;
import org.codehaus.jackson.map.annotate.JsonView;

import java.util.Map;

/**
 * User: wuoti
 * Date: 28.8.2013
 * Time: 12.58
 */
@Embedded
public class OsallistuminenTulos {

    private Osallistuminen osallistuminen;
    private Map<String,String> kuvaus;
    private String laskentaTila;
    private Boolean laskentaTulos;
    private String tekninenKuvaus;

    public Osallistuminen getOsallistuminen() {
        return osallistuminen;
    }

    public void setOsallistuminen(Osallistuminen osallistuminen) {
        this.osallistuminen = osallistuminen;
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

    public Map<String, String> getKuvaus() {
        return kuvaus;
    }

    public void setKuvaus(Map<String, String> kuvaus) {
        this.kuvaus = kuvaus;
    }

    public String getTekninenKuvaus() {
        return tekninenKuvaus;
    }

    public void setTekninenKuvaus(String tekninenKuvaus) {
        this.tekninenKuvaus = tekninenKuvaus;
    }
}
