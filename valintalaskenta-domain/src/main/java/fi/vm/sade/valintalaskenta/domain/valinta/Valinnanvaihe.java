package fi.vm.sade.valintalaskenta.domain.valinta;

import org.apache.commons.lang.builder.ToStringBuilder;
import org.bson.types.ObjectId;
import org.mongodb.morphia.annotations.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Entity(value = "Valinnanvaihe", noClassnameStored = true)
@Indexes(
        @Index(fields = {@Field("hakuOid"), @Field("valinnanvaiheOid")},
                options = @IndexOptions(name = "idx_hakuoid_valinnanvaihe_oid", unique = true))
)
public class Valinnanvaihe {
    private static final Logger LOGGER = LoggerFactory.getLogger(Valinnanvaihe.class);

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
    private List<Valintatapajono> valintatapajonot = new ArrayList<Valintatapajono>();

    @PrePersist
    private void prePersist() {
        createdAt = new Date();
    }

    @PostLoad
    private void jarjestaValintatapajonot() {
        Collections.sort(valintatapajonot, (o1, o2) -> o1.getPrioriteetti() - o2.getPrioriteetti());
    }

    public void setId(ObjectId id) {
        this.id = id;
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

    public void reportDuplicateValintatapajonoOids() {
        Set<String> uniqueJonoOids = new HashSet<>();
        for (Valintatapajono valintatapajono : valintatapajonot) {
            String valintatapajonoOid = valintatapajono.getValintatapajonoOid();
            if (uniqueJonoOids.contains(valintatapajonoOid)) {
                logDuplicate(valintatapajonoOid);
            }
            uniqueJonoOids.add(valintatapajonoOid);
        }
    }

    private void logDuplicate(String valintatapajonoOid) {
        LOGGER.error("Warning: duplicate valintatapajonoOid" + valintatapajonoOid + " detected when saving Valinnanvaihe " + valinnanvaiheOid +
            " . Valintatapajonos are: " + valintatapajonot.stream().map(ToStringBuilder::reflectionToString), new RuntimeException());
    }
}
