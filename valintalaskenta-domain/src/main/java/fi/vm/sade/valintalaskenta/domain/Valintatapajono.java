package fi.vm.sade.valintalaskenta.domain;

import java.util.ArrayList;
import java.util.List;

import com.google.code.morphia.annotations.Embedded;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
@Embedded("Valintatapajono")
public class Valintatapajono {

    private String oid;
    private String nimi;
    private int prioriteetti;
    private int aloituspaikat;
    private boolean siirretaanSijoitteluun;

    @Embedded
    private List<Jarjestyskriteeritulos> jarjestyskriteeritulokset = new ArrayList<Jarjestyskriteeritulos>();

    public String getNimi() {
        return nimi;
    }

    public void setNimi(String nimi) {
        this.nimi = nimi;
    }

    public boolean isSiirretaanSijoitteluun() {
        return siirretaanSijoitteluun;
    }

    public void setSiirretaanSijoitteluun(boolean siirretaanSijoitteluun) {
        this.siirretaanSijoitteluun = siirretaanSijoitteluun;
    }

    public int getPrioriteetti() {
        return prioriteetti;
    }

    public int getAloituspaikat() {
        return aloituspaikat;
    }

    public void setAloituspaikat(int aloituspaikat) {
        this.aloituspaikat = aloituspaikat;
    }

    public void setPrioriteetti(int prioriteetti) {
        this.prioriteetti = prioriteetti;
    }

    public List<Jarjestyskriteeritulos> getJarjestyskriteeritulokset() {
        return jarjestyskriteeritulokset;
    }

    public void setJarjestyskriteeritulokset(List<Jarjestyskriteeritulos> jarjestyskriteeritulokset) {
        this.jarjestyskriteeritulokset = jarjestyskriteeritulokset;
    }

    public String getOid() {
        return oid;
    }

    public void setOid(String oid) {
        this.oid = oid;
    }

}
