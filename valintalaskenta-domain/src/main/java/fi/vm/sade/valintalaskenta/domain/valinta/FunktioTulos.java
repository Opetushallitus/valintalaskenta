package fi.vm.sade.valintalaskenta.domain.valinta;

import org.mongodb.morphia.annotations.Embedded;

@Embedded
public class FunktioTulos {
  private String tunniste;
  private String arvo;
  private String nimiFi;
  private String nimiSv;
  private String nimiEn;
  private boolean omaopintopolku;

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
