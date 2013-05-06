package fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.impl.util;

import fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen;

/**
 * User: wuoti
 * Date: 3.5.2013
 * Time: 9.35
 */
public class HakukohdeValintakoeData {

    private String hakuOid;
    private String hakukohdeOid;
    private String valinnanVaiheOid;
    private int valinnanVaiheJarjestysNro;

    private String valintakoeTunniste;
    private String valintakoeOid;
    private Osallistuminen osallistuminen;

    public String getHakuOid() {
        return hakuOid;
    }

    public void setHakuOid(String hakuOid) {
        this.hakuOid = hakuOid;
    }

    public String getValintakoeTunniste() {
        return valintakoeTunniste;
    }

    public void setValintakoeTunniste(String valintakoeTunniste) {
        this.valintakoeTunniste = valintakoeTunniste;
    }

    public String getHakukohdeOid() {
        return hakukohdeOid;
    }

    public void setHakukohdeOid(String hakukohdeOid) {
        this.hakukohdeOid = hakukohdeOid;
    }

    public String getValinnanVaiheOid() {
        return valinnanVaiheOid;
    }

    public void setValinnanVaiheOid(String valinnanVaiheOid) {
        this.valinnanVaiheOid = valinnanVaiheOid;
    }

    public int getValinnanVaiheJarjestysNro() {
        return valinnanVaiheJarjestysNro;
    }

    public void setValinnanVaiheJarjestysNro(int valinnanVaiheJarjestysNro) {
        this.valinnanVaiheJarjestysNro = valinnanVaiheJarjestysNro;
    }

    public Osallistuminen getOsallistuminen() {
        return osallistuminen;
    }

    public void setOsallistuminen(Osallistuminen osallistuminen) {
        this.osallistuminen = osallistuminen;
    }

    public void setValintakoeOid(String valintakoeOid) {
        this.valintakoeOid = valintakoeOid;
    }

    public String getValintakoeOid() {
        return valintakoeOid;
    }
}
