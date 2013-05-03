package fi.vm.sade.valintalaskenta.domain.valintakoe;

import com.google.code.morphia.annotations.Embedded;

import java.util.ArrayList;
import java.util.List;

/**
 * User: wuoti
 * Date: 2.5.2013
 * Time: 13.29
 */
@Embedded("Hakutoive")
public class Hakutoive {

    private String hakukohdeOid;

    @Embedded
    private List<ValinnanVaihe> valinnanVaiheet = new ArrayList<ValinnanVaihe>();

    public String getHakukohdeOid() {
        return hakukohdeOid;
    }

    public void setHakukohdeOid(String hakukohdeOid) {
        this.hakukohdeOid = hakukohdeOid;
    }

    public List<ValinnanVaihe> getValinnanVaiheet() {
        return valinnanVaiheet;
    }

    public void setValinnanVaiheet(List<ValinnanVaihe> valinnanVaiheet) {
        this.valinnanVaiheet = valinnanVaiheet;
    }
}
