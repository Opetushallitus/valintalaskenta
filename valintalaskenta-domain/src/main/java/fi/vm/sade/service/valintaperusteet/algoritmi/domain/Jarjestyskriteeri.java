package fi.vm.sade.service.valintaperusteet.algoritmi.domain;

import com.google.code.morphia.annotations.Embedded;

/**
 * 
 * @author Jussi Jartamo
 *
 */
@Embedded
public class Jarjestyskriteeri {

    private String oid;
    
    public String getOid() {
        return oid;
    }
    public void setOid(String oid) {
        this.oid = oid;
    }
}
