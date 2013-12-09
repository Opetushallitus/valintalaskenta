package fi.vm.sade.valintalaskenta.domain.dto.valintakoe;

import com.wordnik.swagger.annotations.ApiModel;
import com.wordnik.swagger.annotations.ApiModelProperty;

import java.util.ArrayList;
import java.util.List;

/**
 * User: wuoti
 * Date: 29.8.2013
 * Time: 8.31
 */
@ApiModel(value = "ValintakoeValinnanvaiheDTO", description = "Valintakoevalinnanvaihe")
public class ValintakoeValinnanvaiheDTO {
    @ApiModelProperty(value = "OID", required = true)
    private String valinnanVaiheOid;

    @ApiModelProperty(value = "Vaiheen järjestysluku", required = true)
    private Integer valinnanVaiheJarjestysluku;

    @ApiModelProperty(value = "Valintakokeet")
    private List<ValintakoeDTO> valintakokeet = new ArrayList<ValintakoeDTO>();

    public String getValinnanVaiheOid() {
        return valinnanVaiheOid;
    }

    public void setValinnanVaiheOid(String valinnanVaiheOid) {
        this.valinnanVaiheOid = valinnanVaiheOid;
    }

    public Integer getValinnanVaiheJarjestysluku() {
        return valinnanVaiheJarjestysluku;
    }

    public void setValinnanVaiheJarjestysluku(Integer valinnanVaiheJarjestysluku) {
        this.valinnanVaiheJarjestysluku = valinnanVaiheJarjestysluku;
    }

    public List<ValintakoeDTO> getValintakokeet() {
        return valintakokeet;
    }

    public void setValintakokeet(List<ValintakoeDTO> valintakokeet) {
        this.valintakokeet = valintakokeet;
    }
}
