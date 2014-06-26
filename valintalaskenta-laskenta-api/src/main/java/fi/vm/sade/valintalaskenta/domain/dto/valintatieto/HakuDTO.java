package fi.vm.sade.valintalaskenta.domain.dto.valintatieto;


import fi.vm.sade.valintalaskenta.domain.dto.HakukohdeDTO;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by jukais on 20.3.2014.
 */
public class HakuDTO {
    private String hakuOid;
    private List<HakukohdeDTO> hakukohteet;

    public void setHakuOid(String hakuOid) {
        this.hakuOid = hakuOid;
    }

    public String getHakuOid() {
        return hakuOid;
    }

    public List<HakukohdeDTO> getHakukohteet() {
        if(hakukohteet == null) {
            hakukohteet = new ArrayList<HakukohdeDTO>();
        }
        return hakukohteet;
    }

    public void setHakukohteet(List<HakukohdeDTO> hakukohteet) {
        this.hakukohteet = hakukohteet;
    }
}
