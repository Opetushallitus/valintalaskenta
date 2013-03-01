package fi.vm.sade.valintalaskenta.domain;

import java.util.ArrayList;
import java.util.List;

import com.google.code.morphia.annotations.Embedded;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
@Embedded("Valinnanvaihe")
public class Valinnanvaihe {

    private int jarjestysnumero;
    private String valinnanvaiheoid;

    @Embedded
    private List<Valintatapajono> valintatapajono = new ArrayList<Valintatapajono>();

    public List<Valintatapajono> getValintatapajono() {
        return valintatapajono;
    }

    public String getValinnanvaiheoid() {
        return valinnanvaiheoid;
    }

    public void setValinnanvaiheoid(String valinnanvaiheoid) {
        this.valinnanvaiheoid = valinnanvaiheoid;
    }

    public void setValintatapajono(List<Valintatapajono> valintatapajono) {
        this.valintatapajono = valintatapajono;
    }

    public int getJarjestysnumero() {
        return jarjestysnumero;
    }

    public void setJarjestysnumero(int jarjestysnumero) {
        this.jarjestysnumero = jarjestysnumero;
    }

}
