package fi.vm.sade.valintalaskenta.domain.valintakoe;

import com.google.code.morphia.annotations.Embedded;

/**
 * User: wuoti
 * Date: 2.5.2013
 * Time: 13.32
 */
@Embedded
public class Valintakoe {
    private String valintakoeOid;
    private String valintakoeTunniste;
    private Osallistuminen osallistuminen;

    public String getValintakoeOid() {
        return valintakoeOid;
    }

    public void setValintakoeOid(String valintakoeOid) {
        this.valintakoeOid = valintakoeOid;
    }

    public String getValintakoeTunniste() {
        return valintakoeTunniste;
    }

    public void setValintakoeTunniste(String valintakoeTunniste) {
        this.valintakoeTunniste = valintakoeTunniste;
    }

    public Osallistuminen getOsallistuminen() {
        return osallistuminen;
    }

    public void setOsallistuminen(Osallistuminen osallistuminen) {
        this.osallistuminen = osallistuminen;
    }
}
