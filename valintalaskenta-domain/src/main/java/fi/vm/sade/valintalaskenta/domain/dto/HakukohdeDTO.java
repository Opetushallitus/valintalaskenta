package fi.vm.sade.valintalaskenta.domain.dto;

import fi.vm.sade.valintalaskenta.domain.JsonViews;
import fi.vm.sade.valintalaskenta.domain.Valinnanvaihe;
import org.codehaus.jackson.map.annotate.JsonView;

import java.util.Date;
import java.util.List;

/**
 *
 */
public class HakukohdeDTO {

    @JsonView(JsonViews.Basic.class)
    private Date createdAt;

    @JsonView(JsonViews.Basic.class)
    private String hakuoid;

    @JsonView(JsonViews.Basic.class)
    private String hakukohdeoid;

    @JsonView(JsonViews.Basic.class)
    private List<ValinnanvaiheDTO> valinnanvaihe;

    public String getHakuoid() {
        return hakuoid;
    }

    public void setHakuoid(String hakuoid) {
        this.hakuoid = hakuoid;
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


    public List<ValinnanvaiheDTO> getValinnanvaihe() {
        return valinnanvaihe;
    }

    public void setValinnanvaihe(List<ValinnanvaiheDTO> valinnanvaihe) {
        this.valinnanvaihe = valinnanvaihe;
    }
}