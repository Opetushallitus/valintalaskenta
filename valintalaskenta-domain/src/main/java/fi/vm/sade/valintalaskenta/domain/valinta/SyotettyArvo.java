package fi.vm.sade.valintalaskenta.domain.valinta;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonInclude(JsonInclude.Include.NON_NULL)
public class SyotettyArvo {

  @JsonProperty("tunniste")
  private String tunniste;

  @JsonProperty("arvo")
  private String arvo;

  @JsonProperty("laskennallinenArvo")
  private String laskennallinenArvo;

  @JsonProperty("osallistuminen")
  private String osallistuminen;

  @JsonProperty("tyypinKoodiUri")
  private String tyypinKoodiUri;

  @JsonProperty("tilastoidaan")
  private boolean tilastoidaan;

  public String getTunniste() {
    return tunniste;
  }

  public SyotettyArvo() {}
  ;

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
