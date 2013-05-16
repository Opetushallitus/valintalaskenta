package fi.vm.sade.valintalaskenta.tulos.dto;

/**
 * 
 * @author Jussi Jartamo
 * 
 *         Data transfer object used by Excel template
 */
public class ValintatulosDTO {

    private String nimi;
    private String osoite;

    public String getNimi() {
        return nimi;
    }

    public void setNimi(String nimi) {
        this.nimi = nimi;
    }

    public void setOsoite(String osoite) {
        this.osoite = osoite;
    }

    public String getOsoite() {
        return osoite;
    }

}
