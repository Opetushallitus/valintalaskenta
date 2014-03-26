package fi.vm.sade.valintalaskenta.domain.valintakoe;

import com.google.code.morphia.annotations.Embedded;

/**
 * User: wuoti Date: 2.5.2013 Time: 13.32
 */
@Embedded
public class Valintakoe {
	private String valintakoeOid;
	private String valintakoeTunniste;
	private String nimi;
	private boolean aktiivinen;
	private OsallistuminenTulos osallistuminenTulos;
    private boolean lahetetaankoKoekutsut;

	public boolean isAktiivinen() {
		return aktiivinen;
	}

	public void setAktiivinen(boolean aktiivinen) {
		this.aktiivinen = aktiivinen;
	}

	public String getNimi() {
		return nimi;
	}

	public void setNimi(String nimi) {
		this.nimi = nimi;
	}

	public String getValintakoeOid() {
		return valintakoeOid;
	}

	public void setValintakoeOid(String valintakoeOid) {
		this.valintakoeOid = valintakoeOid;
	}

	public String getValintakoeTunniste() {
		return valintakoeTunniste;
	}

	public void setValintakoeTunniste(String valintakoeTunniste) {
		this.valintakoeTunniste = valintakoeTunniste;
	}

	public OsallistuminenTulos getOsallistuminenTulos() {
		return osallistuminenTulos;
	}

	public void setOsallistuminenTulos(OsallistuminenTulos osallistuminenTulos) {
		this.osallistuminenTulos = osallistuminenTulos;
	}

    public boolean isLahetetaankoKoekutsut() {
        return lahetetaankoKoekutsut;
    }

    public void setLahetetaankoKoekutsut(boolean lahetetaankoKoekutsut) {
        this.lahetetaankoKoekutsut = lahetetaankoKoekutsut;
    }
}
