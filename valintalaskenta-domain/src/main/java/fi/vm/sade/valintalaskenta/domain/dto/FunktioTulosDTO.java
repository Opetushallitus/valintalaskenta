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
@ApiModel(value = "FunktioTulosDTO", description = "Laskennassa saatu funktio tulos")
public class FunktioTulosDTO {
    @ApiModelProperty(value = "Tunniste", required = true)
    @JsonView(JsonViews.Basic.class)
    private String tunniste;

    @ApiModelProperty(value = "Varsinainen arvo, joka laskennassa on saatu", required = true)
    @JsonView(JsonViews.Basic.class)
    private String arvo;

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

}
