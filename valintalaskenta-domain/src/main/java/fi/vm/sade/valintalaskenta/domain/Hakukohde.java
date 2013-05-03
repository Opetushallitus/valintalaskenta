package fi.vm.sade.valintalaskenta.domain;

import com.google.code.morphia.annotations.Embedded;
import com.google.code.morphia.annotations.PrePersist;
import org.codehaus.jackson.map.annotate.JsonView;

import java.util.Date;

/**
 * 
 * @author Jussi Jartamo
 * 
 *         Tätä sulautettua entiteettiä voisi optimoida niin että oidit on
 *         transienttejä ja ne asetetaan versioivasta entiteetistä käsin
 *         palveluhakuja tehtäessä. Eli kun REST-rajapinnasta haetaan tietoa
 *         niin oidit asetetaan paikoilleen -- oideja ei suotta taltioitaisi
 *         kantaan toisteisuuden välttämiseksi.
 * 
 */
@Embedded("Hakukohde")
public class Hakukohde {

    @JsonView(JsonViews.Basic.class)
    private Date createdAt;

    @JsonView(JsonViews.Basic.class)
    private String hakuoid;

    @JsonView(JsonViews.Basic.class)
    private String hakukohdeoid;

    @Embedded
    @JsonView(JsonViews.Basic.class)
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
        return hakukohdeoid;
    }

    public void setOid(String oid) {
        this.hakukohdeoid = oid;
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