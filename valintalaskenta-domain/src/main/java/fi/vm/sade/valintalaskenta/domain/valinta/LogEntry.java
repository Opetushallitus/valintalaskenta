package fi.vm.sade.valintalaskenta.domain.valinta;

import org.mongodb.morphia.annotations.Embedded;
import org.mongodb.morphia.annotations.Id;
import org.bson.types.ObjectId;

import java.util.Date;

/**
 * Created with IntelliJ IDEA.
 * User: jukais
 * Date: 13.8.2013
 * Time: 7.36
 * To change this template use File | Settings | File Templates.
 */
@Embedded("LogEntry")
public class LogEntry {
    @Id
    private ObjectId id;

    private Date luotu;
    private String muokkaaja;
    private String muutos;
    private String selite;

    public Date getLuotu() {
        return luotu;
    }

    public void setLuotu(Date luotu) {
        this.luotu = luotu;
    }

    public String getMuokkaaja() {
        return muokkaaja;
    }

    public void setMuokkaaja(String muokkaaja) {
        this.muokkaaja = muokkaaja;
    }

    public String getSelite() {
        return selite;
    }

    public void setSelite(String selite) {
        this.selite = selite;
    }

    public ObjectId getId() {
        return id;
    }

    public void setId(ObjectId id) {
        this.id = id;
    }

    public String getMuutos() {
        return muutos;
    }

    public void setMuutos(String muutos) {
        this.muutos = muutos;
    }
}
