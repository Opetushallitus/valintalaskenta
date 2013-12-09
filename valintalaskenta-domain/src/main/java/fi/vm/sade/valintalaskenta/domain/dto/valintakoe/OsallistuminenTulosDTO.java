package fi.vm.sade.valintalaskenta.domain.dto.valintakoe;

import com.wordnik.swagger.annotations.ApiModel;
import com.wordnik.swagger.annotations.ApiModelProperty;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen;

/**
 * User: wuoti
 * Date: 29.8.2013
 * Time: 8.32
 */
@ApiModel(value = "OsallistuminenTulosDTO", description = "Osallitumistulos - kertoo, pitääkö hakijan osallistua valintakokeeseen")
public class OsallistuminenTulosDTO {
    @ApiModelProperty(value = "Varsinainen tulos", required = true)
    private Osallistuminen osallistuminen;

    @ApiModelProperty(value = "Kuvaus, esim. virheviesti")
    private String kuvaus;

    @ApiModelProperty(value = "Laskennan palauttama tila")
    private String laskentaTila;

    @ApiModelProperty(value = "Laskennan palauttama tulos")
    private Boolean laskentaTulos;

    public Osallistuminen getOsallistuminen() {
        return osallistuminen;
    }

    public void setOsallistuminen(Osallistuminen osallistuminen) {
        this.osallistuminen = osallistuminen;
    }

    public String getKuvaus() {
        return kuvaus;
    }

    public void setKuvaus(String kuvaus) {
        this.kuvaus = kuvaus;
    }

    public String getLaskentaTila() {
        return laskentaTila;
    }

    public void setLaskentaTila(String laskentaTila) {
        this.laskentaTila = laskentaTila;
    }

    public Boolean getLaskentaTulos() {
        return laskentaTulos;
    }

    public void setLaskentaTulos(Boolean laskentaTulos) {
        this.laskentaTulos = laskentaTulos;
    }
}
