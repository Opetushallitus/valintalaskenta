package fi.vm.sade.valintalaskenta.domain.valintakoe;

import org.mongodb.morphia.annotations.Embedded;
import org.mongodb.morphia.annotations.Indexed;

import java.util.ArrayList;
import java.util.List;

/**
 * User: wuoti
 * Date: 2.5.2013
 * Time: 13.29
 */
@Embedded("Hakutoive")
public class Hakutoive {

    @Indexed
    private String hakukohdeOid;

    @Indexed
    private String laskettavaHakukohdeOid;

    @Embedded
    private List<ValintakoeValinnanvaihe> valinnanVaiheet = new ArrayList<ValintakoeValinnanvaihe>();

    public String getHakukohdeOid() {
        return hakukohdeOid;
    }

    public void setHakukohdeOid(String hakukohdeOid) {
        this.hakukohdeOid = hakukohdeOid;
    }

    public List<ValintakoeValinnanvaihe> getValinnanVaiheet() {
        return valinnanVaiheet;
    }

    public void setValinnanVaiheet(List<ValintakoeValinnanvaihe> valinnanVaiheet) {
        this.valinnanVaiheet = valinnanVaiheet;
    }

    public String getLaskettavaHakukohdeOid() {
        return laskettavaHakukohdeOid;
    }

    public void setLaskettavaHakukohdeOid(String laskettavaHakukohdeOid) {
        this.laskettavaHakukohdeOid = laskettavaHakukohdeOid;
    }
}
