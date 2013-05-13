package fi.vm.sade.valintalaskenta.domain.valintakoe;

import com.google.code.morphia.annotations.Embedded;
import com.google.code.morphia.annotations.Entity;
import com.google.code.morphia.annotations.Id;
import com.google.code.morphia.annotations.PrePersist;
import fi.vm.sade.valintalaskenta.domain.JsonViews;
import org.bson.types.ObjectId;
import org.codehaus.jackson.map.annotate.JsonView;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * User: wuoti
 * Date: 2.5.2013
 * Time: 13.02
 */
@Entity("ValintakoeOsallistuminen")
public class ValintakoeOsallistuminen {

    @Id
    private ObjectId id;

    private String hakuOid;
    private String hakemusOid;
    private String hakijaOid;

    @JsonView(JsonViews.Basic.class)
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
}
