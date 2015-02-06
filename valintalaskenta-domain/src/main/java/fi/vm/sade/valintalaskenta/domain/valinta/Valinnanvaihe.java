package fi.vm.sade.valintalaskenta.domain.valinta;

import org.mongodb.morphia.annotations.*;
import org.bson.types.ObjectId;

import java.util.*;

/**
 * User: wuoti
 * Date: 4.9.2013
 * Time: 10.22
 */
@Entity("Valinnanvaihe")
public class Valinnanvaihe {
    @Id
    private ObjectId id;

    private int jarjestysnumero;

    private Date createdAt;

    @Indexed(unique = false, dropDups = false)
    private String hakuOid;

    @Indexed(unique = false, dropDups = false)
    private String hakukohdeOid;

    @Indexed(unique = false, dropDups = false)
    private String valinnanvaiheOid;

    private String tarjoajaOid;

    private String nimi;

    @Embedded
    private List<Valintatapajono> valintatapajonot = new ArrayList<Valintatapajono>();

    @PrePersist
    private void prePersist() {
        createdAt = new Date();
    }

    @PrePersist
    private void jarjestaValintatapajonot() {
        Collections.sort(valintatapajonot, (o1, o2) -> o1.getPrioriteetti() - o2.getPrioriteetti());
    }

    public int getJarjestysnumero() {
        return jarjestysnumero;
    }

    public void setJarjestysnumero(int jarjestysnumero) {
        this.jarjestysnumero = jarjestysnumero;
    }

    public Date getCreatedAt() {
        return createdAt;
    }

    public void setCreatedAt(Date createdAt) {
        this.createdAt = createdAt;
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

    public String getValinnanvaiheOid() {
        return valinnanvaiheOid;
    }

    public void setValinnanvaiheOid(String valinnanvaiheOid) {
        this.valinnanvaiheOid = valinnanvaiheOid;
    }

    public String getTarjoajaOid() {
        return tarjoajaOid;
    }

    public void setTarjoajaOid(String tarjoajaOid) {
        this.tarjoajaOid = tarjoajaOid;
    }

    public List<Valintatapajono> getValintatapajonot() {
        return valintatapajonot;
    }

    public void setValintatapajonot(List<Valintatapajono> valintatapajonot) {
        this.valintatapajonot = valintatapajonot;
    }

    public String getNimi() {
        return nimi;
    }

    public void setNimi(String nimi) {
        this.nimi = nimi;
    }
}
