package fi.vm.sade.valintalaskenta.domain.valinta;

import org.mongodb.morphia.annotations.*;
import fi.vm.sade.valintalaskenta.domain.JsonViews;
import org.bson.types.ObjectId;
import org.codehaus.jackson.map.annotate.JsonView;

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

    @JsonView(JsonViews.Basic.class)
    private int jarjestysnumero;

    @JsonView(JsonViews.Basic.class)
    private Date createdAt;

    @Indexed(unique = false, dropDups = false)
    @JsonView(JsonViews.Basic.class)
    private String hakuOid;

    @Indexed(unique = false, dropDups = false)
    @JsonView(JsonViews.Basic.class)
    private String hakukohdeOid;

    @Indexed(unique = false, dropDups = false)
    @JsonView(JsonViews.Basic.class)
    private String valinnanvaiheOid;

    @JsonView(JsonViews.Basic.class)
    private String tarjoajaOid;

    @JsonView(JsonViews.Basic.class)
    private String nimi;

    @JsonView(JsonViews.Basic.class)
    @Embedded
    private List<Valintatapajono> valintatapajonot = new ArrayList<Valintatapajono>();

    @PrePersist
    private void prePersist() {
        createdAt = new Date();
    }

    @PrePersist
    private void jarjestaValintatapajonot() {
        Collections.sort(valintatapajonot, new Comparator<Valintatapajono>() {
            @Override
            public int compare(Valintatapajono o1, Valintatapajono o2) {
                return o1.getPrioriteetti() - o2.getPrioriteetti();
            }
        });
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
