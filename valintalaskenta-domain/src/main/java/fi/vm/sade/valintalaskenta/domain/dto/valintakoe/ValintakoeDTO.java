package fi.vm.sade.valintalaskenta.domain.dto.valintakoe;

import com.wordnik.swagger.annotations.ApiModel;
import com.wordnik.swagger.annotations.ApiModelProperty;

/**
 * User: wuoti Date: 29.8.2013 Time: 8.31
 */
@ApiModel(value = "ValintakoeDTO", description = "Valintakoe")
public class ValintakoeDTO {

	@ApiModelProperty(value = "OID", required = true)
	private String valintakoeOid;

	@ApiModelProperty(value = "Kokeen tunniste", required = true)
	private String valintakoeTunniste;

	@ApiModelProperty(value = "Kokeen nimi", required = true)
	private String nimi;

	@ApiModelProperty(value = "Kokeen aktiivisuus", required = true)
	private boolean aktiivinen;

	@ApiModelProperty(value = "Osallistumistulos (pitääkö hakija osallistua ko. kokeeseen)", required = true)
	private OsallistuminenTulosDTO osallistuminenTulos;

    @ApiModelProperty(value = "Lähetetäänkö kokeesta kutsuja", required = true)
    private boolean lahetetaankoKoekutsut;

	public String getValintakoeOid() {
		return valintakoeOid;
	}

	public boolean isAktiivinen() {
		return aktiivinen;
	}

	public void setAktiivinen(boolean aktiivinen) {
		this.aktiivinen = aktiivinen;
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

	public OsallistuminenTulosDTO getOsallistuminenTulos() {
		return osallistuminenTulos;
	}

	public void setOsallistuminenTulos(
			OsallistuminenTulosDTO osallistuminenTulos) {
		this.osallistuminenTulos = osallistuminenTulos;
	}

	public String getNimi() {
		return nimi;
	}

	public void setNimi(String nimi) {
		this.nimi = nimi;
	}

    public boolean isLahetetaankoKoekutsut() {
        return lahetetaankoKoekutsut;
    }

    public void setLahetetaankoKoekutsut(boolean lahetetaankoKoekutsut) {
        this.lahetetaankoKoekutsut = lahetetaankoKoekutsut;
    }
}
