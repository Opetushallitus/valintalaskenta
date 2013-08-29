package fi.vm.sade.valintalaskenta.domain.dto.valintakoe;

import java.util.ArrayList;
import java.util.List;

/**
 * User: wuoti
 * Date: 29.8.2013
 * Time: 8.29
 */
public class HakutoiveDTO {
    private String hakukohdeOid;

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
