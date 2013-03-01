package fi.vm.sade.valintalaskenta.domain;

import java.util.ArrayList;
import java.util.List;

import org.bson.types.ObjectId;

import com.google.code.morphia.annotations.Embedded;
import com.google.code.morphia.annotations.Entity;
import com.google.code.morphia.annotations.Id;
import com.google.code.morphia.annotations.Indexed;

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
    private String valintatapajonooid;
    private Long versio;
    private String nimi;
    private int prioriteetti;
    private int aloituspaikat;
    private boolean siirretaanSijoitteluun;

    @Embedded
    private List<Jarjestyskriteeritulos> jarjestyskriteeritulokset = new ArrayList<Jarjestyskriteeritulos>();

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

    public List<Jarjestyskriteeritulos> getJarjestyskriteeritulokset() {
        return jarjestyskriteeritulokset;
    }

    public void setJarjestyskriteeritulokset(List<Jarjestyskriteeritulos> jarjestyskriteeritulokset) {
        this.jarjestyskriteeritulokset = jarjestyskriteeritulokset;
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
}
