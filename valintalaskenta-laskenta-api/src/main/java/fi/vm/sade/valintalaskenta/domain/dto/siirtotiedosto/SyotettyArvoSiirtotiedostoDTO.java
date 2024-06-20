package fi.vm.sade.valintalaskenta.domain.dto.siirtotiedosto;

public class SyotettyArvoSiirtotiedostoDTO {
  private String tunniste;

  private String arvo;

  private String laskennallinenArvo;

  private String osallistuminen;

  private String tyypinKoodiUri;

  private boolean tilastoidaan;

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

  public String getLaskennallinenArvo() {
    return laskennallinenArvo;
  }

  public void setLaskennallinenArvo(String laskennallinenArvo) {
    this.laskennallinenArvo = laskennallinenArvo != null ? laskennallinenArvo.intern() : null;
  }

  public String getOsallistuminen() {
    return osallistuminen;
  }

  public void setOsallistuminen(String osallistuminen) {
    this.osallistuminen = osallistuminen != null ? osallistuminen.intern() : null;
  }

  public String getTyypinKoodiUri() {
    return tyypinKoodiUri;
  }

  public void setTyypinKoodiUri(String tyypinKoodiUri) {
    this.tyypinKoodiUri = tyypinKoodiUri != null ? tyypinKoodiUri.intern() : null;
  }

  public boolean isTilastoidaan() {
    return tilastoidaan;
  }

  public void setTilastoidaan(boolean tilastoidaan) {
    this.tilastoidaan = tilastoidaan;
  }
}
