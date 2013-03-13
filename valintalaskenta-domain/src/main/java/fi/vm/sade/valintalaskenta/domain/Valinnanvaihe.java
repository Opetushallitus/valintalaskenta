package fi.vm.sade.valintalaskenta.domain;

import com.google.code.morphia.annotations.Embedded;
import com.google.code.morphia.annotations.PrePersist;
import com.google.code.morphia.annotations.Reference;
import org.codehaus.jackson.map.annotate.JsonView;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
@Embedded("Valinnanvaihe")
public class Valinnanvaihe {

    @JsonView(JsonViews.Basic.class)
    private int jarjestysnumero;

    @JsonView(JsonViews.Basic.class)
    private String valinnanvaiheoid;

    @JsonView(JsonViews.Basic.class)
    private Date createdAt;

    @Reference
    @JsonView(JsonViews.Basic.class)
    private List<Valintatapajono> valintatapajono = new ArrayList<Valintatapajono>();

    public List<Valintatapajono> getValintatapajono() {
        return valintatapajono;
    }

    public String getValinnanvaiheoid() {
        return valinnanvaiheoid;
    }

    public void setValinnanvaiheoid(String valinnanvaiheoid) {
        this.valinnanvaiheoid = valinnanvaiheoid;
    }

    public void setValintatapajono(List<Valintatapajono> valintatapajono) {
        this.valintatapajono = valintatapajono;
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

    @PrePersist
    private void prePersist() {
        if (createdAt == null) {
            createdAt = new Date();
        }
    }
}
