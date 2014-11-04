package fi.vm.sade.valintalaskenta.domain.valinta;

import java.util.ArrayList;
import java.util.List;

import org.codehaus.jackson.map.annotate.JsonView;

import org.mongodb.morphia.annotations.Embedded;

import fi.vm.sade.valintalaskenta.domain.JsonViews;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.Tasasijasaanto;
import org.mongodb.morphia.annotations.Indexed;

/**
 * User: wuoti Date: 4.9.2013 Time: 10.25
 */
@Embedded
public class Valintatapajono {

	@JsonView(JsonViews.Basic.class)
    @Indexed
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
    private Boolean kaikkiEhdonTayttavatHyvaksytaan;

    @JsonView(JsonViews.Basic.class)
    private Boolean kaytetaanValintalaskentaa;

    @JsonView(JsonViews.Basic.class)
    private Boolean poissaOlevaTaytto;

    @JsonView(JsonViews.Basic.class)
    private Boolean valmisSijoiteltavaksi = true;

	@JsonView(JsonViews.Basic.class)
	@Embedded
	private List<Jonosija> jonosijat = new ArrayList<Jonosija>();

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

	public List<Jonosija> getJonosijat() {
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
