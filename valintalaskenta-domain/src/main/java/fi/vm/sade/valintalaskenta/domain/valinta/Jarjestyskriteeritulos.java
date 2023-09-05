package fi.vm.sade.valintalaskenta.domain.valinta;

import java.math.BigDecimal;
import java.util.Map;
import org.apache.commons.lang3.builder.ToStringBuilder;

public class Jarjestyskriteeritulos {

  private int prioriteetti;

  private BigDecimal arvo;

  private JarjestyskriteerituloksenTila tila;

  private Map<String, String> kuvaus;

  private String nimi;

  private String tekninenKuvaus;

  private String historia;

  public int getPrioriteetti() {
    return prioriteetti;
  }

  public void setPrioriteetti(int prioriteetti) {
    this.prioriteetti = prioriteetti;
  }

  public BigDecimal getArvo() {
    return arvo;
  }

  public void setArvo(BigDecimal arvo) {
    this.arvo = arvo;
  }

  public JarjestyskriteerituloksenTila getTila() {
    return tila;
  }

  public void setTila(JarjestyskriteerituloksenTila tila) {
    this.tila = tila;
  }

  public String getHistoria() {
    return historia;
  }

  public void setHistoria(String historia) {
    this.historia = historia;
  }

  public String getNimi() {
    return nimi;
  }

  public void setNimi(String nimi) {
    this.nimi = nimi;
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
