package fi.vm.sade.service.valintaperusteet.algoritmi.domain;

import java.util.ArrayList;
import java.util.List;

import org.bson.types.ObjectId;

import com.google.code.morphia.annotations.Entity;
import com.google.code.morphia.annotations.Id;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
@Entity("Hakemus")
@Deprecated
public class Hakemus {

    @Id
    private ObjectId id;

    private String oid;

    private List<Jarjestyskriteeritulos> jarjestyskriteeritulos = new ArrayList<Jarjestyskriteeritulos>();

    public List<Jarjestyskriteeritulos> getJarjestyskriteeritulos() {
        return jarjestyskriteeritulos;
    }

    public void setJarjestyskriteeritulos(List<Jarjestyskriteeritulos> jarjestyskriteeritulos) {
        this.jarjestyskriteeritulos = jarjestyskriteeritulos;
    }

    public String getOid() {
        return oid;
    }

    public void setOid(String oid) {
        this.oid = oid;
    }

}
