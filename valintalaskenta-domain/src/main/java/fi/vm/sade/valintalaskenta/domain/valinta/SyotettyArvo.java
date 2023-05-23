package fi.vm.sade.valintalaskenta.domain.valinta;

import dev.morphia.annotations.Entity;

@Entity
public class SyotettyArvo {
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
    this.tunniste = tunniste;
  }

  public String getArvo() {
    return arvo;
  }

  public void setArvo(String arvo) {
    this.arvo = arvo;
  }

  public String getLaskennallinenArvo() {
    return laskennallinenArvo;
  }

  public void setLaskennallinenArvo(String laskennallinenArvo) {
    this.laskennallinenArvo = laskennallinenArvo;
  }

  public String getOsallistuminen() {
    return osallistuminen;
  }

  public void setOsallistuminen(String osallistuminen) {
    this.osallistuminen = osallistuminen;
  }

  public String getTyypinKoodiUri() {
    return tyypinKoodiUri;
  }

  public void setTyypinKoodiUri(String tyypinKoodiUri) {
    this.tyypinKoodiUri = tyypinKoodiUri;
  }

  public boolean isTilastoidaan() {
    return tilastoidaan;
  }

  public void setTilastoidaan(boolean tilastoidaan) {
    this.tilastoidaan = tilastoidaan;
  }
}
