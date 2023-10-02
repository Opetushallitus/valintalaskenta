package fi.vm.sade.valintalaskenta.domain.valinta;

import com.fasterxml.jackson.annotation.JsonProperty;

public class FunktioTulos {

  @JsonProperty("tunniste")
  private String tunniste;

  @JsonProperty("arvo")
  private String arvo;

  @JsonProperty("nimiFi")
  private String nimiFi;

  @JsonProperty("nimiSv")
  private String nimiSv;

  @JsonProperty("nimiEn")
  private String nimiEn;

  @JsonProperty("omaopintopolku")
  private boolean omaopintopolku;

  public FunktioTulos() {};

  public String getTunniste() {
    return tunniste;
  }

  public void setTunniste(String tunniste) {
    this.tunniste = tunniste;
  }

  public String getArvo() {
    return arvo;
  }

  public void setArvo(String arvo) {
    this.arvo = arvo;
  }

  public String getNimiFi() {
    return nimiFi;
  }

  public void setNimiFi(String nimiFi) {
    this.nimiFi = nimiFi;
  }

  public String getNimiSv() {
    return nimiSv;
  }

  public void setNimiSv(String nimiSv) {
    this.nimiSv = nimiSv;
  }

  public String getNimiEn() {
    return nimiEn;
  }

  public void setNimiEn(String nimiEn) {
    this.nimiEn = nimiEn;
  }

  public boolean isOmaopintopolku() {
    return omaopintopolku;
  }

  public void setOmaopintopolku(boolean omaopintopolku) {
    this.omaopintopolku = omaopintopolku;
  }
}
