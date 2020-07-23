package fi.vm.sade.valintalaskenta.domain.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;

@ApiModel(value = "SyotettyArvoDTO", description = "Laskennassa käytetty syötettävä arvo")
public class SyotettyArvoDTO {
    @ApiModelProperty(value = "Tunniste", required = true)
    private String tunniste;

    @ApiModelProperty(value = "Varsinainen arvo, joka hakemukselle on syötetty", required = true)
    private String arvo;

    @ApiModelProperty(value = "Laskennassa käytetty arvo eli esim. jos " +
            "hakemuksen arvo on laskennassa konvertoitu toiseksi arvoksi", required = true)
    private String laskennallinenArvo;

    @ApiModelProperty(value = "Arvon osallistumistieto", required = true)
    private String osallistuminen;

    @ApiModelProperty(value = "Arvon tyypin koodisto uri", required = true)
    private String tyypinKoodiUri;

    @ApiModelProperty(value = "Tilastoidaanko tieto vai", required = true)
    private boolean tilastoidaan;

    public String getTunniste() {
        return tunniste;
    }

    public void setTunniste(String tunniste) {
        this.tunniste = tunniste != null ? tunniste.intern() : null;
    }

    public String getArvo() {
        return arvo;
    }

    public void setArvo(String arvo) {
        this.arvo = arvo != null ? arvo.intern() : null;
    }

    public String getLaskennallinenArvo() {
        return laskennallinenArvo;
    }

    public void setLaskennallinenArvo(String laskennallinenArvo) {
        this.laskennallinenArvo = laskennallinenArvo != null ? laskennallinenArvo.intern() : null;
    }

    public String getOsallistuminen() {
        return osallistuminen;
    }

    public void setOsallistuminen(String osallistuminen) {
        this.osallistuminen = osallistuminen != null ? osallistuminen.intern() : null;
    }

    public String getTyypinKoodiUri() {
        return tyypinKoodiUri;
    }

    public void setTyypinKoodiUri(String tyypinKoodiUri) {
        this.tyypinKoodiUri = tyypinKoodiUri != null ? tyypinKoodiUri.intern() : null;
    }

    public boolean isTilastoidaan() {
        return tilastoidaan;
    }

    public void setTilastoidaan(boolean tilastoidaan) {
        this.tilastoidaan = tilastoidaan;
    }
}
