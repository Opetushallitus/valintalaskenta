package fi.vm.sade.valintalaskenta.domain.dto.valintakoe;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * User: wuoti
 * Date: 29.8.2013
 * Time: 8.28
 */
@ApiModel(value ="valintalaskenta.domain.dto.valintakoe.ValintakoeOsallistuminenDTO", description = "Valintakoeosallistuminen")
public class ValintakoeOsallistuminenDTO {

    @ApiModelProperty(value="Haku OID", required = true)
    private String hakuOid;

    @ApiModelProperty(value="Hakemus OID", required = true)
    private String hakemusOid;

    @ApiModelProperty(value="Hakija OID")
    private String hakijaOid;

    @ApiModelProperty(value="Etunimi")
    private String etunimi;

    @ApiModelProperty(value="Sukunimi")
    private String sukunimi;

    @ApiModelProperty(value="Luontiajankohta")
    private Date createdAt;

    @ApiModelProperty(value="Hakutoiveet")
    private List<HakutoiveDTO> hakutoiveet = new ArrayList<HakutoiveDTO>();


    public String getHakuOid() {
        return hakuOid;
    }

    public void setHakuOid(String hakuOid) {
        this.hakuOid = hakuOid;
    }

    public String getHakemusOid() {
        return hakemusOid;
    }

    public void setHakemusOid(String hakemusOid) {
        this.hakemusOid = hakemusOid;
    }

    public String getHakijaOid() {
        return hakijaOid;
    }

    public void setHakijaOid(String hakijaOid) {
        this.hakijaOid = hakijaOid;
    }

    public String getEtunimi() {
        return etunimi;
    }

    public void setEtunimi(String etunimi) {
        this.etunimi = etunimi;
    }

    public String getSukunimi() {
        return sukunimi;
    }

    public void setSukunimi(String sukunimi) {
        this.sukunimi = sukunimi;
    }

    public Date getCreatedAt() {
        return createdAt;
    }

    public void setCreatedAt(Date createdAt) {
        this.createdAt = createdAt;
    }

    public List<HakutoiveDTO> getHakutoiveet() {
        return hakutoiveet;
    }

    public void setHakutoiveet(List<HakutoiveDTO> hakutoiveet) {
        this.hakutoiveet = hakutoiveet;
    }
}
