package fi.vm.sade.valintalaskenta.domain.dto;

import com.wordnik.swagger.annotations.ApiModel;
import com.wordnik.swagger.annotations.ApiModelProperty;
import fi.vm.sade.valintalaskenta.domain.JsonViews;
import org.codehaus.jackson.map.annotate.JsonView;

/**
 * User: wuoti
 * Date: 23.9.2013
 * Time: 12.11
 */
@ApiModel(value = "SyotettyArvoDTO", description = "Laskennassa käytetty syötettävä arvo")
public class SyotettyArvoDTO {
    @ApiModelProperty(value = "Tunniste", required = true)
    @JsonView(JsonViews.Basic.class)
    private String tunniste;

    @ApiModelProperty(value = "Varsinainen arvo, joka hakemukselle on syötetty", required = true)
    @JsonView(JsonViews.Basic.class)
    private String arvo;

    @ApiModelProperty(value = "Laskennassa käytetty arvo eli esim. jos " +
            "hakemuksen arvo on laskennassa konvertoitu toiseksi arvoksi", required = true)
    @JsonView(JsonViews.Basic.class)
    private String laskennallinenArvo;

    @ApiModelProperty(value = "Arvon osallistumistieto", required = true)
    @JsonView(JsonViews.Basic.class)
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
