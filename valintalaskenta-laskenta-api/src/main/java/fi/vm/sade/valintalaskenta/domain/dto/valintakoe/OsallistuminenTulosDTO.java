package fi.vm.sade.valintalaskenta.domain.dto.valintakoe;

import fi.vm.sade.valintalaskenta.domain.valintakoe.Osallistuminen;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.Map;

@Schema(
    name = "OsallistuminenTulosDTO",
    description = "Osallitumistulos - kertoo, pitääkö hakijan osallistua valintakokeeseen")
public class OsallistuminenTulosDTO {
  @Schema(name = "Varsinainen tulos", required = true)
  private Osallistuminen osallistuminen;

  @Schema(name = "Monikielinen kuvaus, esim. virheviesti")
  private Map<String, String> kuvaus;

  @Schema(name = "Laskennan palauttama tila")
  private String laskentaTila;

  @Schema(name = "Laskennan palauttama tulos")
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
