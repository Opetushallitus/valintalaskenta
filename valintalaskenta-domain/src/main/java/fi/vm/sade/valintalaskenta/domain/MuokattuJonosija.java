package fi.vm.sade.valintalaskenta.domain;

import com.google.code.morphia.annotations.Embedded;
import com.google.code.morphia.annotations.Entity;
import com.google.code.morphia.annotations.Id;
import com.google.code.morphia.annotations.Indexed;
import org.bson.types.ObjectId;
import org.codehaus.jackson.map.annotate.JsonView;

import java.util.ArrayList;
import java.util.List;
import java.util.TreeMap;

/**
 * User: kkammone Date: 13.5.2013 Time: 9:50
 */
@Entity("MuokattuJonosija")
public class MuokattuJonosija {

    @Id
    private ObjectId id;

    @Indexed(unique = false, dropDups = false)
    @JsonView(JsonViews.Basic.class)
    private String hakuOid;

    @Indexed(unique = false, dropDups = false)
    @JsonView(JsonViews.Basic.class)
    private String valintatapajonoOid;

    @JsonView(JsonViews.Basic.class)
    private String hakemusOid;

    @JsonView(JsonViews.Basic.class)
    private String hakijaoid;

    @JsonView(JsonViews.Basic.class)
    private int prioriteetti; // hakutoive

    @JsonView(JsonViews.Basic.class)
    private boolean harkinnanvarainen = false;

    @Embedded
    private TreeMap<Integer, Jarjestyskriteeritulos> jarjestyskriteerit = new TreeMap<Integer, Jarjestyskriteeritulos>();

    public String getHakijaoid() {
        return hakijaoid;
    }

    public void setHakijaoid(String hakijaoid) {
        this.hakijaoid = hakijaoid;
    }

    public TreeMap<Integer, Jarjestyskriteeritulos> getJarjestyskriteerit() {
        return jarjestyskriteerit;
    }

    public void setJarjestyskriteerit(TreeMap<Integer, Jarjestyskriteeritulos> jarjestyskriteerit) {
        this.jarjestyskriteerit = jarjestyskriteerit;
    }

    public int getPrioriteetti() {
        return prioriteetti;
    }

    public void setPrioriteetti(int prioriteetti) {
        this.prioriteetti = prioriteetti;
    }

    public boolean isHarkinnanvarainen() {
        return harkinnanvarainen;
    }

    public void setHarkinnanvarainen(boolean harkinnanvarainen) {
        this.harkinnanvarainen = harkinnanvarainen;
    }


    public String getValintatapajonoOid() {
        return valintatapajonoOid;
    }

    public void setValintatapajonoOid(String valintatapajonoOid) {
        this.valintatapajonoOid = valintatapajonoOid;
    }

    public String getHakemusOid() {
        return hakemusOid;
    }

    public void setHakemusOid(String hakemusOid) {
        this.hakemusOid = hakemusOid;
    }

    public String getHakuOid() {
        return hakuOid;
    }

    public void setHakuOid(String hakuOid) {
        this.hakuOid = hakuOid;
    }
}
