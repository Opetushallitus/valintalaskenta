package fi.vm.sade.valintalaskenta.domain.valinta;

import org.bson.types.ObjectId;
import org.mongodb.morphia.annotations.*;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

@Entity(value = "Valinnanvaihe", noClassnameStored = true)
@Indexes(
    @Index(name = "idx_hakuoid_valinnanvaihe_oid", value = "hakuOid, valinnanvaiheOid", unique = true)
)
public class ValinnanvaiheMigrationDTO {
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

    @Reference
    private List<ValintatapajonoMigrationDTO> valintatapajonot = new ArrayList<ValintatapajonoMigrationDTO>();

    @PrePersist
    private void prePersist() {
        createdAt = new Date();
    }

    @PostLoad
    private void jarjestaValintatapajonot() {
        Collections.sort(valintatapajonot, (o1, o2) -> o1.getPrioriteetti() - o2.getPrioriteetti());
    }

    public ObjectId getId() {
        return this.id;
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

    public List<ValintatapajonoMigrationDTO> getValintatapajonot() {
        return valintatapajonot;
    }

    public void setValintatapajonot(List<ValintatapajonoMigrationDTO> valintatapajonot) {
        this.valintatapajonot = valintatapajonot;
    }

    public String getNimi() {
        return nimi;
    }

    public void setNimi(String nimi) {
        this.nimi = nimi;
    }
}
