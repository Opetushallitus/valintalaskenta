package fi.vm.sade.service.valintaperusteet.algoritmi.domain;

import java.util.HashSet;
import java.util.Set;

import com.google.code.morphia.annotations.Embedded;

/**
 * 
 * @author Jussi Jartamo
 *
 */
@Embedded
public class Jono {

    private String oid;

    @Embedded
    private Jarjestyskriteeri jarjestyskriteeri;

    @Embedded
    private Set<Tulos> tulokset = new HashSet<Tulos>();

    public Set<Tulos> getTulokset() {
        return tulokset;
    }

    public void setTulokset(Set<Tulos> tulokset) {
        this.tulokset = tulokset;
    }

    public Jarjestyskriteeri getJarjestyskriteeri() {
        return jarjestyskriteeri;
    }

    public void setJarjestyskriteeri(Jarjestyskriteeri jarjestyskriteeri) {
        this.jarjestyskriteeri = jarjestyskriteeri;
    }

    public String getOid() {
        return oid;
    }

    public void setOid(String oid) {
        this.oid = oid;
    }

}
