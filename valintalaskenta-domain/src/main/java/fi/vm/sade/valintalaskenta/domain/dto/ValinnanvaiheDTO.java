package fi.vm.sade.valintalaskenta.domain.dto;

import com.google.code.morphia.annotations.Embedded;
import com.google.code.morphia.annotations.PrePersist;
import com.google.code.morphia.annotations.Reference;
import fi.vm.sade.valintalaskenta.domain.JsonViews;
import fi.vm.sade.valintalaskenta.domain.Valintatapajono;
import org.codehaus.jackson.map.annotate.JsonView;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 *
 */
public class ValinnanvaiheDTO {

    @JsonView(JsonViews.Basic.class)
    private int jarjestysnumero;

    @JsonView(JsonViews.Basic.class)
    private String valinnanvaiheoid;

    @JsonView(JsonViews.Basic.class)
    private Date createdAt;

    @JsonView(JsonViews.Basic.class)
    private List<ValintatapajonoDTO> valintatapajono = new ArrayList<ValintatapajonoDTO>();

    public List<ValintatapajonoDTO> getValintatapajono() {
        return valintatapajono;
    }

    public String getValinnanvaiheoid() {
        return valinnanvaiheoid;
    }

    public void setValinnanvaiheoid(String valinnanvaiheoid) {
        this.valinnanvaiheoid = valinnanvaiheoid;
    }

    public void setValintatapajono(List<ValintatapajonoDTO> valintatapajono) {
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


}
