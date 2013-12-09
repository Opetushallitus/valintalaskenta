package fi.vm.sade.valintalaskenta.domain.dto.valintakoe;

import com.wordnik.swagger.annotations.ApiModel;
import com.wordnik.swagger.annotations.ApiModelProperty;

/**
 * User: wuoti
 * Date: 29.8.2013
 * Time: 8.31
 */
@ApiModel(value = "ValintakoeDTO", description = "Valintakoe")
public class ValintakoeDTO {

    @ApiModelProperty(value = "OID", required = true)
    private String valintakoeOid;

    @ApiModelProperty(value = "Kokeen tunniste", required = true)
    private String valintakoeTunniste;

    @ApiModelProperty(value = "Osallistumistulos (pitääkö hakija osallistua ko. kokeeseen)", required = true)
    private OsallistuminenTulosDTO osallistuminenTulos;

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

    public OsallistuminenTulosDTO getOsallistuminenTulos() {
        return osallistuminenTulos;
    }

    public void setOsallistuminenTulos(OsallistuminenTulosDTO osallistuminenTulos) {
        this.osallistuminenTulos = osallistuminenTulos;
    }
}
