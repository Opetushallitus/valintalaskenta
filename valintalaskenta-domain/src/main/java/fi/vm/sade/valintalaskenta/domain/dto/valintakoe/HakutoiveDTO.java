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

    private List<ValinnanvaiheDTO> valinnanVaiheet = new ArrayList<ValinnanvaiheDTO>();

    public String getHakukohdeOid() {
        return hakukohdeOid;
    }

    public void setHakukohdeOid(String hakukohdeOid) {
        this.hakukohdeOid = hakukohdeOid;
    }

    public List<ValinnanvaiheDTO> getValinnanVaiheet() {
        return valinnanVaiheet;
    }

    public void setValinnanVaiheet(List<ValinnanvaiheDTO> valinnanVaiheet) {
        this.valinnanVaiheet = valinnanVaiheet;
    }
}
