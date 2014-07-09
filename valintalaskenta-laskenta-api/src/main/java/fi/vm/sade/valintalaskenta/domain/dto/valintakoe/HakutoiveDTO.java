package fi.vm.sade.valintalaskenta.domain.dto.valintakoe;

import com.wordnik.swagger.annotations.ApiModel;
import com.wordnik.swagger.annotations.ApiModelProperty;

import java.util.ArrayList;
import java.util.List;

/**
 * User: wuoti
 * Date: 29.8.2013
 * Time: 8.29
 */
@ApiModel(value="HakutoiveDTO", description = "Hakutoive")
public class HakutoiveDTO {

    @ApiModelProperty(value="Hakutoiveen OID", required = true)
    private String hakukohdeOid;

    @ApiModelProperty(value="Valintakoevalinnan vaiheet")
    private List<ValintakoeValinnanvaiheDTO> valinnanVaiheet = new ArrayList<ValintakoeValinnanvaiheDTO>();

    public String getHakukohdeOid() {
        return hakukohdeOid;
    }

    public void setHakukohdeOid(String hakukohdeOid) {
        this.hakukohdeOid = hakukohdeOid;
    }

    public List<ValintakoeValinnanvaiheDTO> getValinnanVaiheet() {
        return valinnanVaiheet;
    }

    public void setValinnanVaiheet(List<ValintakoeValinnanvaiheDTO> valinnanVaiheet) {
        this.valinnanVaiheet = valinnanVaiheet;
    }
}