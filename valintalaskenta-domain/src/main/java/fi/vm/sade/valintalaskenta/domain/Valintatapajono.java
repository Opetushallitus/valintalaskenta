package fi.vm.sade.valintalaskenta.domain;

import com.google.code.morphia.annotations.Embedded;
import com.google.code.morphia.annotations.Entity;
import com.google.code.morphia.annotations.Id;
import com.google.code.morphia.annotations.Indexed;
import org.bson.types.ObjectId;
import org.codehaus.jackson.map.annotate.JsonView;

import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author Jussi Jartamo
 *
 */
@Entity("Valintatapajono")
// ei haluta tupla indexiä koska haku tehdään lähes aina pelkällä oid:lla
// @Indexes(@Index(name = "valintatapajonoIndex", unique = true, value =
// "valintatapajonooid, versio"))
public class Valintatapajono implements Comparable<Valintatapajono> {

    @Id
    private ObjectId id;

    @Indexed(unique = false, dropDups = false)
    @JsonView(JsonViews.Basic.class)
    private String valintatapajonooid;

    @JsonView(JsonViews.Basic.class)
    private Long versio;

    @JsonView(JsonViews.Basic.class)
    private String nimi;

    @JsonView(JsonViews.Basic.class)
    private int prioriteetti;

    @JsonView(JsonViews.Basic.class)
    private int aloituspaikat;

    @JsonView(JsonViews.Basic.class)
    private boolean siirretaanSijoitteluun;

    @JsonView(JsonViews.Basic.class)
    private Tasasijasaanto tasasijasaanto;

    @JsonView(JsonViews.Basic.class)
    private Boolean eiVarasijatayttoa;

    @JsonView(JsonViews.Basic.class)
    @Embedded
    private List <Jonosija> jonosijat = new ArrayList<Jonosija>();

    public String getNimi() {
        return nimi;
    }

    public Long getVersio() {
        return versio;
    }

    public void setVersio(Long versio) {
        this.versio = versio;
    }

    public void setNimi(String nimi) {
        this.nimi = nimi;
    }

    public boolean isSiirretaanSijoitteluun() {
        return siirretaanSijoitteluun;
    }

    public void setSiirretaanSijoitteluun(boolean siirretaanSijoitteluun) {
        this.siirretaanSijoitteluun = siirretaanSijoitteluun;
    }

    public int getPrioriteetti() {
        return prioriteetti;
    }

    public int getAloituspaikat() {
        return aloituspaikat;
    }

    public void setAloituspaikat(int aloituspaikat) {
        this.aloituspaikat = aloituspaikat;
    }

    public void setPrioriteetti(int prioriteetti) {
        this.prioriteetti = prioriteetti;
    }


    public String getOid() {
        return valintatapajonooid;
    }

    public void setOid(String oid) {
        this.valintatapajonooid = oid;
    }

    public ObjectId getId() {
        return id;
    }

    public void setId(ObjectId id) {
        this.id = id;
    }

    public int compareTo(Valintatapajono o) {
        if (equals(o)) {
            return 0;
        }
        return versio.compareTo(o.versio);
    }

    public boolean equals(Object obj) {
        if (obj instanceof Valintatapajono) {
            Valintatapajono vtj = (Valintatapajono) obj;
            return this == vtj;
        }
        return false;
    }

    public int hashCode() {
        return versio.intValue();
    }

    public Tasasijasaanto getTasasijasaanto() {
        return tasasijasaanto;
    }

    public void setTasasijasaanto(Tasasijasaanto tasasijasaanto) {
        this.tasasijasaanto = tasasijasaanto;
    }

    public List<Jonosija> getJonosijat() {
        return jonosijat;
    }

    public void setJonosijat(List<Jonosija> jonosijat) {
        this.jonosijat = jonosijat;
    }

    public Boolean getEiVarasijatayttoa() {
        return eiVarasijatayttoa;
    }

    public void setEiVarasijatayttoa(Boolean eiVarasijatayttoa) {
        this.eiVarasijatayttoa = eiVarasijatayttoa;
    }
}
