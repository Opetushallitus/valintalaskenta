package fi.vm.sade.valintalaskenta.domain.dto.valintakoe;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;

import java.util.ArrayList;
import java.util.List;

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
