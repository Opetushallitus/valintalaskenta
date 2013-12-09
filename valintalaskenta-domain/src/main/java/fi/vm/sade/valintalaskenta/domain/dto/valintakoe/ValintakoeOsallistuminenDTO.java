package fi.vm.sade.valintalaskenta.domain.dto.valintakoe;

import com.wordnik.swagger.annotations.ApiModel;
import com.wordnik.swagger.annotations.ApiModelProperty;
import fi.vm.sade.valintalaskenta.domain.JsonViews;
import org.codehaus.jackson.map.annotate.JsonView;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * User: wuoti
 * Date: 29.8.2013
 * Time: 8.28
 */
@ApiModel(value ="ValintakoeOsallistuminenDTO", description = "Valintakoeosallistuminen")
public class ValintakoeOsallistuminenDTO {

    @ApiModelProperty(value="Haku OID", required = true)
    @JsonView(JsonViews.Basic.class)
    private String hakuOid;

    @ApiModelProperty(value="Hakemus OID", required = true)
    @JsonView(JsonViews.Basic.class)
    private String hakemusOid;

    @ApiModelProperty(value="Hakija OID")
    @JsonView(JsonViews.Basic.class)
    private String hakijaOid;

    @ApiModelProperty(value="Etunimi")
    @JsonView(JsonViews.Basic.class)
    private String etunimi;

    @ApiModelProperty(value="Sukunimi")
    @JsonView(JsonViews.Basic.class)
    private String sukunimi;

    @ApiModelProperty(value="Luontiajankohta")
    @JsonView(JsonViews.Basic.class)
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
