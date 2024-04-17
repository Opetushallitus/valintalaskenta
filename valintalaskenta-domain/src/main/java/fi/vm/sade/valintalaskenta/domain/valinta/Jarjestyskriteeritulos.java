package fi.vm.sade.valintalaskenta.domain.valinta;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.math.BigDecimal;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import org.apache.commons.lang3.builder.ToStringBuilder;

@JsonInclude(JsonInclude.Include.NON_NULL)
public class Jarjestyskriteeritulos {

  @JsonProperty("prioriteetti")
  private int prioriteetti;

  @JsonProperty("arvo")
  private BigDecimal arvo;

  @JsonProperty("tila")
  private JarjestyskriteerituloksenTila tila;

  @JsonProperty("kuvausFI")
  private String kuvausFI = "";

  @JsonProperty("kuvausSV")
  private String kuvausSV = "";

  @JsonProperty("kuvausEN")
  private String kuvausEN = "";

  @JsonProperty("nimi")
  private String nimi;

  @JsonProperty("tekninenKuvaus")
  private String tekninenKuvaus;

  @JsonProperty("historia")
  private UUID historia;

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

  public String getNimi() {
    return nimi;
  }

  public void setNimi(String nimi) {
    this.nimi = nimi;
  }

  public Map<String, String> getKuvaus() {
    Map<String, String> kuvaus = new HashMap<>();
    if (getKuvausFI() != null) {
      kuvaus.put("FI", getKuvausFI());
    }
    if (getKuvausSV() != null) {
      kuvaus.put("SV", getKuvausSV());
    }
    if (getKuvausEN() != null) {
      kuvaus.put("EN", getKuvausEN());
    }
    return kuvaus;
  }

  public void setKuvaus(Map<String, String> kuvaus) {
    this.setKuvausFI(kuvaus.get("FI"));
    this.setKuvausEN(kuvaus.get("EN"));
    this.setKuvausSV(kuvaus.get("SV"));
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

  public String getKuvausEN() {
    return kuvausEN;
  }

  public void setKuvausEN(String kuvausEn) {
    this.kuvausEN = kuvausEn;
  }

  public String getKuvausFI() {
    return kuvausFI;
  }

  public void setKuvausFI(String kuvausFI) {
    this.kuvausFI = kuvausFI;
  }

  public String getKuvausSV() {
    return kuvausSV;
  }

  public void setKuvausSV(String kuvausSV) {
    this.kuvausSV = kuvausSV;
  }

  public UUID getHistoria() {
    return historia;
  }

  public void setHistoria(UUID historia) {
    this.historia = historia;
  }
}
