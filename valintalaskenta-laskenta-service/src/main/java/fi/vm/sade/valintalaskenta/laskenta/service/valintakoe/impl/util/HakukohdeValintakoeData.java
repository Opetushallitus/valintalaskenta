package fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.impl.util;

import fi.vm.sade.valintalaskenta.domain.valintakoe.OsallistuminenTulos;

/**
 * User: wuoti Date: 3.5.2013 Time: 9.35
 */
public class HakukohdeValintakoeData {

	private String hakuOid;
	private String hakukohdeOid;
	private String valinnanVaiheOid;
	private int valinnanVaiheJarjestysNro;

	private String valintakoeTunniste;
	private String valintakoeOid;
	private String nimi;
	private OsallistuminenTulos osallistuminenTulos;
    private boolean lahetetaankoKoekutsut;

	public String getNimi() {
		return nimi;
	}

	public void setNimi(String nimi) {
		this.nimi = nimi;
	}

	public String getHakuOid() {
		return hakuOid;
	}

	public void setHakuOid(String hakuOid) {
		this.hakuOid = hakuOid;
	}

	public String getValintakoeTunniste() {
		return valintakoeTunniste;
	}

	public void setValintakoeTunniste(String valintakoeTunniste) {
		this.valintakoeTunniste = valintakoeTunniste;
	}

	public String getHakukohdeOid() {
		return hakukohdeOid;
	}

	public void setHakukohdeOid(String hakukohdeOid) {
		this.hakukohdeOid = hakukohdeOid;
	}

	public String getValinnanVaiheOid() {
		return valinnanVaiheOid;
	}

	public void setValinnanVaiheOid(String valinnanVaiheOid) {
		this.valinnanVaiheOid = valinnanVaiheOid;
	}

	public int getValinnanVaiheJarjestysNro() {
		return valinnanVaiheJarjestysNro;
	}

	public void setValinnanVaiheJarjestysNro(int valinnanVaiheJarjestysNro) {
		this.valinnanVaiheJarjestysNro = valinnanVaiheJarjestysNro;
	}

	public void setValintakoeOid(String valintakoeOid) {
		this.valintakoeOid = valintakoeOid;
	}

	public String getValintakoeOid() {
		return valintakoeOid;
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
