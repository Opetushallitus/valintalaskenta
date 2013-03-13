package fi.vm.sade.valintalaskenta.domain;

import com.google.code.morphia.annotations.Embedded;
import com.google.code.morphia.annotations.Reference;
import org.codehaus.jackson.map.annotate.JsonView;

import java.util.ArrayList;
import java.util.List;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
@Embedded("Valinnanvaihe")
public class Valinnanvaihe {

    @JsonView(JsonViews.Basic.class)
    private int jarjestysnumero;

    @JsonView(JsonViews.Basic.class)
    private String valinnanvaiheoid;

    @Reference
    @JsonView(JsonViews.Basic.class)
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
