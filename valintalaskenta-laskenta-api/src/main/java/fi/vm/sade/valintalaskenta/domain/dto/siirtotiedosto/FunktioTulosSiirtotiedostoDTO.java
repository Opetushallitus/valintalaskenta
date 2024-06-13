package fi.vm.sade.valintalaskenta.domain.dto.siirtotiedosto;

public class FunktioTulosSiirtotiedostoDTO {
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
    this.tunniste = tunniste != null ? tunniste.intern() : null;
  }

  public String getArvo() {
    return arvo;
  }

  public void setArvo(String arvo) {
    this.arvo = arvo != null ? arvo.intern() : null;
  }

  public String getNimiFi() {
    return nimiFi;
  }

  public void setNimiFi(String nimiFi) {
    this.nimiFi = nimiFi != null ? nimiFi.intern() : null;
  }

  public String getNimiSv() {
    return nimiSv;
  }

  public void setNimiSv(String nimiSv) {
    this.nimiSv = nimiSv != null ? nimiSv.intern() : null;
  }

  public String getNimiEn() {
    return nimiEn;
  }

  public void setNimiEn(String nimiEn) {
    this.nimiEn = nimiEn != null ? nimiEn.intern() : null;
  }

  public boolean isOmaopintopolku() {
    return omaopintopolku;
  }

  public void setOmaopintopolku(boolean omaopintopolku) {
    this.omaopintopolku = omaopintopolku;
  }
}
