package fi.vm.sade.valintalaskenta.domain.dto;

import io.swagger.v3.oas.annotations.media.Schema;

@Schema(name = "SyotettyArvoDTO", description = "Laskennassa käytetty syötettävä arvo")
public class SyotettyArvoDTO {
  @Schema(title = "Tunniste", required = true)
  private String tunniste;

  @Schema(title = "Varsinainen arvo, joka hakemukselle on syötetty", required = true)
  private String arvo;

  @Schema(
      title =
          "Laskennassa käytetty arvo eli esim. jos "
              + "hakemuksen arvo on laskennassa konvertoitu toiseksi arvoksi",
      required = true)
  private String laskennallinenArvo;

  @Schema(title = "Arvon osallistumistieto", required = true)
  private String osallistuminen;

  @Schema(title = "Arvon tyypin koodisto uri", required = true)
  private String tyypinKoodiUri;

  @Schema(title = "Tilastoidaanko tieto vai", required = true)
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
