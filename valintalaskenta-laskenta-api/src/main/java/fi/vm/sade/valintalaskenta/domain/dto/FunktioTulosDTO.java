package fi.vm.sade.valintalaskenta.domain.dto;

import io.swagger.v3.oas.annotations.media.Schema;

@Schema(name = "FunktioTulosDTO", description = "Laskennassa saatu funktio tulos")
public class FunktioTulosDTO {
  @Schema(name = "Tunniste", required = true)
  private String tunniste;

  @Schema(name = "Varsinainen arvo, joka laskennassa on saatu", required = true)
  private String arvo;

  @Schema(name = "Suomenkielinen nimi", required = false)
  private String nimiFi;

  @Schema(name = "Ruotsinkielinen nimi", required = false)
  private String nimiSv;

  @Schema(name = "Englanninkielinen nimi", required = false)
  private String nimiEn;

  @Schema(name = "Näytetäänkö Omassa Opintopolussa", required = false)
  private boolean omaopintopolku;

  public FunktioTulosDTO() {}

  public FunktioTulosDTO(String tunniste, String arvo) {
    this.tunniste = tunniste != null ? tunniste.intern() : null;
    this.arvo = arvo != null ? arvo.intern() : null;
  }

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
