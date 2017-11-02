package fi.vm.sade.valintalaskenta.domain.valintakoe;

import org.apache.commons.lang3.builder.ToStringBuilder;
import org.bson.types.ObjectId;
import org.mongodb.morphia.annotations.Embedded;
import org.mongodb.morphia.annotations.Entity;
import org.mongodb.morphia.annotations.Field;
import org.mongodb.morphia.annotations.Id;
import org.mongodb.morphia.annotations.Index;
import org.mongodb.morphia.annotations.IndexOptions;
import org.mongodb.morphia.annotations.Indexed;
import org.mongodb.morphia.annotations.Indexes;
import org.mongodb.morphia.annotations.PrePersist;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

@Entity("ValintakoeOsallistuminen")
@Indexes({
        @Index(fields = {@Field("hakuOid")}, options = @IndexOptions(name = "idx_haku"))
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
    private List<Hakutoive> hakutoiveet = new ArrayList<>();

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

    @Override
    public String toString() {
        return ToStringBuilder.reflectionToString(this);
    }
}
