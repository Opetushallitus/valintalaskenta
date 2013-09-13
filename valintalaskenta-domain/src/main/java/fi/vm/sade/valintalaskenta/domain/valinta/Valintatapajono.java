package fi.vm.sade.valintalaskenta.domain.valinta;

import com.google.code.morphia.annotations.Embedded;
import fi.vm.sade.valintalaskenta.domain.JsonViews;
import org.codehaus.jackson.map.annotate.JsonView;

import java.util.ArrayList;
import java.util.List;

/**
 * User: wuoti
 * Date: 4.9.2013
 * Time: 10.25
 */
@Embedded
public class Valintatapajono {

    @JsonView(JsonViews.Basic.class)
    private String valintatapajonoOid;

    @JsonView(JsonViews.Basic.class)
    private String nimi;

    @JsonView(JsonViews.Basic.class)
    private int prioriteetti;

    @JsonView(JsonViews.Basic.class)
    private int aloituspaikat;

    @JsonView(JsonViews.Basic.class)
    private boolean siirretaanSijoitteluun;

    @JsonView(JsonViews.Basic.class)
    private Tasasijasaanto tasasijasaanto;

    @JsonView(JsonViews.Basic.class)
    private Boolean eiVarasijatayttoa;

    @JsonView(JsonViews.Basic.class)
    @Embedded
    private List<Jonosija> jonosijat = new ArrayList<Jonosija>();

    public String getValintatapajonoOid() {
        return valintatapajonoOid;
    }

    public void setValintatapajonoOid(String valintatapajonoOid) {
        this.valintatapajonoOid = valintatapajonoOid;
    }

    public String getNimi() {
        return nimi;
    }

    public void setNimi(String nimi) {
        this.nimi = nimi;
    }

    public int getPrioriteetti() {
        return prioriteetti;
    }

    public void setPrioriteetti(int prioriteetti) {
        this.prioriteetti = prioriteetti;
    }

    public int getAloituspaikat() {
        return aloituspaikat;
    }

    public void setAloituspaikat(int aloituspaikat) {
        this.aloituspaikat = aloituspaikat;
    }

    public boolean isSiirretaanSijoitteluun() {
        return siirretaanSijoitteluun;
    }

    public void setSiirretaanSijoitteluun(boolean siirretaanSijoitteluun) {
        this.siirretaanSijoitteluun = siirretaanSijoitteluun;
    }

    public Tasasijasaanto getTasasijasaanto() {
        return tasasijasaanto;
    }

    public void setTasasijasaanto(Tasasijasaanto tasasijasaanto) {
        this.tasasijasaanto = tasasijasaanto;
    }

    public Boolean getEiVarasijatayttoa() {
        return eiVarasijatayttoa;
    }

    public void setEiVarasijatayttoa(Boolean eiVarasijatayttoa) {
        this.eiVarasijatayttoa = eiVarasijatayttoa;
    }

    public List<Jonosija> getJonosijat() {
        return jonosijat;
    }

    public void setJonosijat(List<Jonosija> jonosijat) {
        this.jonosijat = jonosijat;
    }
}