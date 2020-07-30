package fi.vm.sade.valintalaskenta.domain.valintakoe;

import java.util.Map;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.mongodb.morphia.annotations.Embedded;

@Embedded
public class OsallistuminenTulos {
  private Osallistuminen osallistuminen;
  private Map<String, String> kuvaus;
  private String laskentaTila;
  private Boolean laskentaTulos;
  private String tekninenKuvaus;

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

  public String getTekninenKuvaus() {
    return tekninenKuvaus;
  }

  public void setTekninenKuvaus(String tekninenKuvaus) {
    this.tekninenKuvaus = tekninenKuvaus;
  }

  @Override
  public String toString() {
    return ToStringBuilder.reflectionToString(this);
  }
}
