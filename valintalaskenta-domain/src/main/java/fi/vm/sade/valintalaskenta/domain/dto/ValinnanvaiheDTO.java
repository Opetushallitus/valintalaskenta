package fi.vm.sade.valintalaskenta.domain.dto;

import com.wordnik.swagger.annotations.ApiModel;
import com.wordnik.swagger.annotations.ApiModelProperty;
import fi.vm.sade.valintalaskenta.domain.JsonViews;
import org.codehaus.jackson.map.annotate.JsonView;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 *
 */
@ApiModel(value = "ValinnanvaiheDTO", description = "Valinnan vaihe")
public class ValinnanvaiheDTO {

    @ApiModelProperty(value = "Järjestysnumero", required = true)
    @JsonView(JsonViews.Basic.class)
    private int jarjestysnumero;

    @ApiModelProperty(value = "Valinnan vaiheen OID", required = true)
    @JsonView(JsonViews.Basic.class)
    private String valinnanvaiheoid;

    @ApiModelProperty(value = "Luomisajankohta", required = true)
    @JsonView(JsonViews.Basic.class)
    private Date createdAt;

    @ApiModelProperty(value = "Valintatapajonot", required = true)
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
