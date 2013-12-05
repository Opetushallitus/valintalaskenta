package fi.vm.sade.valintalaskenta.domain.dto;

import com.wordnik.swagger.annotations.ApiModel;
import com.wordnik.swagger.annotations.ApiModelProperty;
import fi.vm.sade.valintalaskenta.domain.JsonViews;
import org.codehaus.jackson.map.annotate.JsonView;

import java.util.ArrayList;
import java.util.List;

/**
 *
 */
@ApiModel(value = "Hakukohde", description = "Hakukohde")
public class HakukohdeDTO {
    @ApiModelProperty(value = "Haku OID", required = true)
    @JsonView(JsonViews.Basic.class)
    private String hakuoid;

    @ApiModelProperty(value = "Tarjoaja OID", required = true)
    @JsonView(JsonViews.Basic.class)
    private String tarjoajaoid;

    @ApiModelProperty(value = "Hakukohde OID", required = true)
    @JsonView(JsonViews.Basic.class)
    private String hakukohdeoid;

    @ApiModelProperty(value = "Valinnan vaiheet", required = true)
    @JsonView(JsonViews.Basic.class)
    private List<ValinnanvaiheDTO> valinnanvaihe = new ArrayList<ValinnanvaiheDTO>();

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

    public List<ValinnanvaiheDTO> getValinnanvaihe() {
        return valinnanvaihe;
    }

    public void setValinnanvaihe(List<ValinnanvaiheDTO> valinnanvaihe) {
        this.valinnanvaihe = valinnanvaihe;
    }

    public String getTarjoajaoid() {
        return tarjoajaoid;
    }

    public void setTarjoajaoid(String tarjoajaoid) {
        this.tarjoajaoid = tarjoajaoid;
    }
}