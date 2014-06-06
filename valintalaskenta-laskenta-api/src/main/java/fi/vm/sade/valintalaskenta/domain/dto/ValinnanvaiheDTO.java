package fi.vm.sade.valintalaskenta.domain.dto;

import com.wordnik.swagger.annotations.ApiModel;
import com.wordnik.swagger.annotations.ApiModelProperty;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.ValintakoeDTO;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 *
 */
@ApiModel(value = "ValinnanvaiheDTO", description = "Valinnan vaihe")
public class ValinnanvaiheDTO {

    @ApiModelProperty(value = "Järjestysnumero", required = true)
    private int jarjestysnumero;

    @ApiModelProperty(value = "Valinnan vaiheen OID", required = true)
    private String valinnanvaiheoid;

    @ApiModelProperty(value = "Haun OID", required = true)
    private String hakuOid;

    @ApiModelProperty(value = "Valinnan vaiheen nimi")
    private String nimi;

    @ApiModelProperty(value = "Luomisajankohta", required = true)
    private Date createdAt;

    @ApiModelProperty(value = "Valintatapajonot", required = true)
    private List<ValintatapajonoDTO> valintatapajonot = new ArrayList<ValintatapajonoDTO>();

    @ApiModelProperty(value = "Valintakokeet")
    private List<ValintakoeDTO> valintakokeet = new ArrayList<ValintakoeDTO>();

    public List<ValintatapajonoDTO> getValintatapajonot() {
        return valintatapajonot;
    }

    public String getValinnanvaiheoid() {
        return valinnanvaiheoid;
    }

    public void setValinnanvaiheoid(String valinnanvaiheoid) {
        this.valinnanvaiheoid = valinnanvaiheoid;
    }

    public void setValintatapajonot(List<ValintatapajonoDTO> valintatapajonot) {
        this.valintatapajonot = valintatapajonot;
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

    public String getNimi() {
        return nimi;
    }

    public void setNimi(String nimi) {
        this.nimi = nimi;
    }

    public List<ValintakoeDTO> getValintakokeet() {
        return valintakokeet;
    }

    public void setValintakokeet(List<ValintakoeDTO> valintakokeet) {
        this.valintakokeet = valintakokeet;
    }

    public String getHakuOid() {
        return hakuOid;
    }

    public void setHakuOid(String hakuOid) {
        this.hakuOid = hakuOid;
    }
}
