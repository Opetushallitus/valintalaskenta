package fi.vm.sade.valintalaskenta.domain;

import java.util.Date;

import com.google.code.morphia.annotations.Embedded;
import com.google.code.morphia.annotations.PrePersist;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
@Embedded("Hakukohde")
public class Hakukohde {

    private Date createdAt;
    private String hakuoid;
    private String oid;

    @Embedded
    private Valinnanvaihe valinnanvaihe;

    public Valinnanvaihe getValinnanvaihe() {
        return valinnanvaihe;
    }

    public String getHakuoid() {
        return hakuoid;
    }

    public void setHakuoid(String hakuoid) {
        this.hakuoid = hakuoid;
    }

    public void setValinnanvaihe(Valinnanvaihe valinnanvaihe) {
        this.valinnanvaihe = valinnanvaihe;
    }

    public String getOid() {
        return oid;
    }

    public void setOid(String oid) {
        this.oid = oid;
    }

    public Date getCreatedAt() {
        return createdAt;
    }

    public void setCreatedAt(Date createdAt) {
        this.createdAt = createdAt;
    }

    @PrePersist
    private void prePersist() {
        if (createdAt == null) {
            createdAt = new Date();
        }
    }
}