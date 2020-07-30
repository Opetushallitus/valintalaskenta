package fi.vm.sade.valintalaskenta.domain.dto.valintakoe;

import fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.util.Map;

@ApiModel(
    value = "OsallistuminenTulosDTO",
    description = "Osallitumistulos - kertoo, pitääkö hakijan osallistua valintakokeeseen")
public class OsallistuminenTulosDTO {
  @ApiModelProperty(value = "Varsinainen tulos", required = true)
  private Osallistuminen osallistuminen;

  @ApiModelProperty(value = "Monikielinen kuvaus, esim. virheviesti")
  private Map<String, String> kuvaus;

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

  public Map<String, String> getKuvaus() {
    return kuvaus;
  }

  public void setKuvaus(Map<String, String> kuvaus) {
    this.kuvaus = kuvaus;
  }
}
