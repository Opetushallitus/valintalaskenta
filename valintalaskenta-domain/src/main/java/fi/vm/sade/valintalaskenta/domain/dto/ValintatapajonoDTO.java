package fi.vm.sade.valintalaskenta.domain.dto;

import fi.vm.sade.valintalaskenta.domain.JsonViews;
import fi.vm.sade.valintalaskenta.domain.valinta.Tasasijasaanto;
import org.codehaus.jackson.map.annotate.JsonView;

import java.util.ArrayList;
import java.util.List;

/**
 * @author Jussi Jartamo
 */

public class ValintatapajonoDTO {

    @JsonView(JsonViews.Basic.class)
    private String valintatapajonooid;

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
    private List<JonosijaDTO> jonosijat = new ArrayList<JonosijaDTO>();

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


    public String getOid() {
        return valintatapajonooid;
    }

    public void setOid(String oid) {
        this.valintatapajonooid = oid;
    }

    public boolean equals(Object obj) {
        if (obj instanceof ValintatapajonoDTO) {
            ValintatapajonoDTO vtj = (ValintatapajonoDTO) obj;
            return this == vtj;
        }
        return false;
    }

    public Tasasijasaanto getTasasijasaanto() {
        return tasasijasaanto;
    }

    public void setTasasijasaanto(Tasasijasaanto tasasijasaanto) {
        this.tasasijasaanto = tasasijasaanto;
    }

    public List<JonosijaDTO> getJonosijat() {
        return jonosijat;
    }

    public void setJonosijat(List<JonosijaDTO> jonosijat) {
        this.jonosijat = jonosijat;
    }

    public Boolean getEiVarasijatayttoa() {
        return eiVarasijatayttoa;
    }

    public void setEiVarasijatayttoa(Boolean eiVarasijatayttoa) {
        this.eiVarasijatayttoa = eiVarasijatayttoa;
    }
}
