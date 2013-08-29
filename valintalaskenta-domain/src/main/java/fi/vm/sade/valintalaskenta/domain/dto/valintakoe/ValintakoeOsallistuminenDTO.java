package fi.vm.sade.valintalaskenta.domain.dto.valintakoe;

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
public class ValintakoeOsallistuminenDTO {

    @JsonView(JsonViews.Basic.class)
    private String hakuOid;

    @JsonView(JsonViews.Basic.class)
    private String hakemusOid;

    @JsonView(JsonViews.Basic.class)
    private String hakijaOid;

    @JsonView(JsonViews.Basic.class)
    private String etunimi;

    @JsonView(JsonViews.Basic.class)
    private String sukunimi;

    @JsonView(JsonViews.Basic.class)
    private Date createdAt;

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
