package fi.vm.sade.valintalaskenta.domain;

import com.google.code.morphia.annotations.Embedded;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
@Embedded("Hakukohde")
public class Hakukohde {

    private String hakuoid;
    private String oid;

    @Embedded
    private Valinnanvaihe valinnanvaihe;// = new ArrayList<Valinnanvaihe>();

    public Valinnanvaihe getValinnanvaihe() {
        return valinnanvaihe;
    }

    public String getHakuoid() {
        return hakuoid;
    }

    public void setHakuoid(String hakuoid) {
        this.hakuoid = hakuoid;
    }

    public void setValinnanvaihe(Valinnanvaihe valinnanvaihe) {
        this.valinnanvaihe = valinnanvaihe;
    }

    public String getOid() {
        return oid;
    }

    public void setOid(String oid) {
        this.oid = oid;
    }

}