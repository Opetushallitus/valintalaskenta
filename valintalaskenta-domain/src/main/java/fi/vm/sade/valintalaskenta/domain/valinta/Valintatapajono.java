package fi.vm.sade.valintalaskenta.domain.valinta;

import java.util.ArrayList;
import java.util.List;

import org.bson.types.ObjectId;
import org.mongodb.morphia.annotations.*;

import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.Tasasijasaanto;

@Entity("Valintatapajono")
public class Valintatapajono {
    @Id
    private ObjectId id;

    @Indexed
    private String valintatapajonoOid;

    private String nimi;

    private int prioriteetti;

    private int aloituspaikat;

    private boolean siirretaanSijoitteluun;

    private Tasasijasaanto tasasijasaanto;

    private Boolean eiVarasijatayttoa;

    private Boolean kaikkiEhdonTayttavatHyvaksytaan;

    private Boolean kaytetaanValintalaskentaa;

    private Boolean poissaOlevaTaytto;

    private Boolean valmisSijoiteltavaksi = true;

    private Boolean kaytetaanKokonaispisteita;

    private List<ObjectId> jonosijaIdt = new ArrayList<>();

    @Transient
    private List<Jonosija> jonosijat;

    private Long sijoitteluajoId;

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

    public List<ObjectId> getJonosijaIdt() {
        return jonosijaIdt;
    }

    public void setJonosijaIdt(List<ObjectId> jonosijaIdt) {
        this.jonosijaIdt = jonosijaIdt;
    }

    public List<Jonosija> getJonosijat() {
        if (null == jonosijat) {
            throw new IllegalStateException("Jonosijat not loaded");
        }
        return jonosijat;
    }

    public void setJonosijat(List<Jonosija> jonosijat) {
        this.jonosijat = jonosijat;
    }

    public Boolean getKaikkiEhdonTayttavatHyvaksytaan() {
        return kaikkiEhdonTayttavatHyvaksytaan;
    }

    public void setKaikkiEhdonTayttavatHyvaksytaan(Boolean kaikkiEhdonTayttavatHyvaksytaan) {
        this.kaikkiEhdonTayttavatHyvaksytaan = kaikkiEhdonTayttavatHyvaksytaan;
    }

    public Boolean getPoissaOlevaTaytto() {
        return poissaOlevaTaytto;
    }

    public void setPoissaOlevaTaytto(Boolean poissaOlevaTaytto) {
        this.poissaOlevaTaytto = poissaOlevaTaytto;
    }

    public Boolean getKaytetaanValintalaskentaa() {
        return kaytetaanValintalaskentaa;
    }

    public void setKaytetaanValintalaskentaa(Boolean kaytetaanValintalaskentaa) {
        this.kaytetaanValintalaskentaa = kaytetaanValintalaskentaa;
    }

    public Boolean getKaytetaanKokonaispisteita() {
        return kaytetaanKokonaispisteita;
    }

    public void setKaytetaanKokonaispisteita(Boolean kaytetaanKokonaispisteita) {
        this.kaytetaanKokonaispisteita = kaytetaanKokonaispisteita;
    }

    public Boolean getValmisSijoiteltavaksi() {
        return valmisSijoiteltavaksi;
    }

    public void setValmisSijoiteltavaksi(Boolean valmisSijoiteltavaksi) {
        this.valmisSijoiteltavaksi = valmisSijoiteltavaksi;
    }

    public Long getSijoitteluajoId() {
        return sijoitteluajoId;
    }

    public void setSijoitteluajoId(Long sijoitteluajoId) {
        this.sijoitteluajoId = sijoitteluajoId;
    }
}
