package fi.vm.sade.service.valintaperusteet.algoritmi.domain;

import org.bson.types.ObjectId;

import com.google.code.morphia.annotations.Embedded;
import com.google.code.morphia.annotations.Id;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
@Embedded("Hakukohde")
public class Hakukohde {

    @Id
    private ObjectId id;

    private String oid;

    @Embedded
    private Valinnanvaihe valinnanvaihe;// = new ArrayList<Valinnanvaihe>();

    public Valinnanvaihe getValinnanvaihe() {
        return valinnanvaihe;
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