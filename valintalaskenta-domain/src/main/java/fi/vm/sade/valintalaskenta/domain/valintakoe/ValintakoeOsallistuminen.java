package fi.vm.sade.valintalaskenta.domain.valintakoe;

import com.google.code.morphia.annotations.Embedded;
import com.google.code.morphia.annotations.Entity;
import com.google.code.morphia.annotations.Id;
import org.bson.types.ObjectId;

import java.util.ArrayList;
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
}
