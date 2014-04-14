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

    @ApiModelProperty(value = "Suomenkielinen nimi", required = false)
    @JsonView(JsonViews.Basic.class)
    private String nimiFi;

    @ApiModelProperty(value = "Ruotsinkielinen nimi", required = false)
    @JsonView(JsonViews.Basic.class)
    private String nimiSv;

    @ApiModelProperty(value = "Englanninkielinen nimi", required = false)
    @JsonView(JsonViews.Basic.class)
    private String nimiEn;

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

    public String getNimiFi() {
        return nimiFi;
    }

    public void setNimiFi(String nimiFi) {
        this.nimiFi = nimiFi;
    }

    public String getNimiSv() {
        return nimiSv;
    }

    public void setNimiSv(String nimiSv) {
        this.nimiSv = nimiSv;
    }

    public String getNimiEn() {
        return nimiEn;
    }

    public void setNimiEn(String nimiEn) {
        this.nimiEn = nimiEn;
    }
}
