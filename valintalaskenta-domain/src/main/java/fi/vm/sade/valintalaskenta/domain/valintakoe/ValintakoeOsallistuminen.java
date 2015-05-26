package fi.vm.sade.valintalaskenta.domain.valintakoe;

import org.mongodb.morphia.annotations.*;
import org.bson.types.ObjectId;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * User: wuoti
 * Date: 2.5.2013
 * Time: 13.02
 */
@Entity("ValintakoeOsallistuminen")
@Indexes({
        @Index(name = "idx_haku", value = "hakuOid")
})
public class ValintakoeOsallistuminen {
    @Id
    private ObjectId id;

    private String hakuOid;
    @Indexed(unique = true)
    private String hakemusOid;

    private String hakijaOid;

    private String etunimi;

    private String sukunimi;

    private Date createdAt;

    @Embedded
    private List<Hakutoive> hakutoiveet = new ArrayList<Hakutoive>();

    public ObjectId getId() {
        return id;
    }

    public void setId(ObjectId id) {
        this.id = id;
    }

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

    public List<Hakutoive> getHakutoiveet() {
        return hakutoiveet;
    }

    public void setHakutoiveet(List<Hakutoive> hakutoiveet) {
        this.hakutoiveet = hakutoiveet;
    }

    @PrePersist
    private void prePersist() {
        createdAt = new Date();
    }

    public Date getCreatedAt() {
        return createdAt;
    }

    public void setCreatedAt(Date createdAt) {
        this.createdAt = createdAt;
    }

    public String getSukunimi() {
        return sukunimi;
    }

    public void setSukunimi(String sukunimi) {
        this.sukunimi = sukunimi;
    }

    public String getEtunimi() {
        return etunimi;
    }

    public void setEtunimi(String etunimi) {
        this.etunimi = etunimi;
    }
}
