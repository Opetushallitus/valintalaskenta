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

    public String getTyypinKoodiUri() {
        return tyypinKoodiUri;
    }

    public void setTyypinKoodiUri(String tyypinKoodiUri) {
        this.tyypinKoodiUri = tyypinKoodiUri;
    }

    public boolean isTilastoidaan() {
        return tilastoidaan;
    }

    public void setTilastoidaan(boolean tilastoidaan) {
        this.tilastoidaan = tilastoidaan;
    }
}
