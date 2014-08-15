package fi.vm.sade.valintalaskenta.domain.valinta;

import org.mongodb.morphia.annotations.Embedded;
import org.mongodb.morphia.annotations.Entity;
import org.mongodb.morphia.annotations.Id;
import org.mongodb.morphia.annotations.Indexed;
import fi.vm.sade.valintalaskenta.domain.JsonViews;
import org.bson.types.ObjectId;
import org.codehaus.jackson.map.annotate.JsonView;

import java.util.ArrayList;
import java.util.List;

/**
 * User: kkammone Date: 13.5.2013 Time: 9:50
 */
@Entity("MuokattuJonosija")
public class MuokattuJonosija {

    @Id
    private ObjectId id;

    @Indexed(unique = false, dropDups = false)
    @JsonView(JsonViews.Basic.class)
    private String hakukohdeOid;

    @Indexed(unique = false, dropDups = false)
    @JsonView(JsonViews.Basic.class)
    private String hakuOid;

    @Indexed(unique = false, dropDups = false)
    @JsonView(JsonViews.Basic.class)
    private String valintatapajonoOid;

    @JsonView(JsonViews.Basic.class)
    private String hakemusOid;

    @JsonView(JsonViews.Basic.class)
    private Integer prioriteetti; // hakutoive

    @Embedded
    private List<Jarjestyskriteeritulos> jarjestyskriteerit = new ArrayList<Jarjestyskriteeritulos>();

    @Embedded
    private List<LogEntry> logEntries = new ArrayList<LogEntry>();

    public List<Jarjestyskriteeritulos> getJarjestyskriteerit() {
        return jarjestyskriteerit;
    }

    public void setJarjestyskriteerit(List<Jarjestyskriteeritulos> jarjestyskriteerit) {
        this.jarjestyskriteerit = jarjestyskriteerit;
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

    public String getHakukohdeOid() {
        return hakukohdeOid;
    }

    public void setHakukohdeOid(String hakukohdeOid) {
        this.hakukohdeOid = hakukohdeOid;
    }

    public Integer getPrioriteetti() {
        return prioriteetti;
    }

    public void setPrioriteetti(Integer prioriteetti) {
        this.prioriteetti = prioriteetti;
    }

    public List<LogEntry> getLogEntries() {
        return logEntries;
    }

    public void setLogEntries(List<LogEntry> logEntries) {
        this.logEntries = logEntries;
    }
}
