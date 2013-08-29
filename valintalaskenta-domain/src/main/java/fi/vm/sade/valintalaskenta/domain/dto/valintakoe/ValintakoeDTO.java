package fi.vm.sade.valintalaskenta.domain.dto.valintakoe;

/**
 * User: wuoti
 * Date: 29.8.2013
 * Time: 8.31
 */
public class ValintakoeDTO {
    private String valintakoeOid;
    private String valintakoeTunniste;
    private OsallistuminenTulosDTO osallistuminenTulos;

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

    public OsallistuminenTulosDTO getOsallistuminenTulos() {
        return osallistuminenTulos;
    }

    public void setOsallistuminenTulos(OsallistuminenTulosDTO osallistuminenTulos) {
        this.osallistuminenTulos = osallistuminenTulos;
    }
}
