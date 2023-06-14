package fi.vm.sade.valintalaskenta.domain.dto.valintakoe;

import io.swagger.v3.oas.annotations.media.Schema;

@Schema(name = "ValintakoeDTO", description = "Valintakoe")
public class ValintakoeDTO {

  @Schema(title = "OID", required = true)
  private String valintakoeOid;

  @Schema(title = "Kokeen tunniste", required = true)
  private String valintakoeTunniste;

  @Schema(title = "Kokeen nimi", required = true)
  private String nimi;

  @Schema(title = "Kokeen aktiivisuus", required = true)
  private boolean aktiivinen;

  @Schema(title = "Osallistumistulos (pitääkö hakija osallistua ko. kokeeseen)", required = true)
  private OsallistuminenTulosDTO osallistuminenTulos;

  @Schema(title = "Lähetetäänkö kokeesta kutsuja", required = true)
  private boolean lahetetaankoKoekutsut;

  @Schema(title = "Kutsutaanko kaikki kokeeseen", required = true)
  private Boolean kutsutaankoKaikki;

  @Schema(title = "Kutsuttavien määrä", required = true)
  private Integer kutsuttavienMaara;

  public Boolean getKutsutaankoKaikki() {
    return kutsutaankoKaikki;
  }

  public void setKutsutaankoKaikki(Boolean kutsutaankoKaikki) {
    this.kutsutaankoKaikki = kutsutaankoKaikki;
  }

  public String getValintakoeOid() {
    return valintakoeOid;
  }

  public boolean isAktiivinen() {
    return aktiivinen;
  }

  public void setAktiivinen(boolean aktiivinen) {
    this.aktiivinen = aktiivinen;
  }

  public void setValintakoeOid(String valintakoeOid) {
    this.valintakoeOid = valintakoeOid;
  }

  public String getValintakoeTunniste() {
    return valintakoeTunniste;
  }

  public void setValintakoeTunniste(String valintakoeTunniste) {
    this.valintakoeTunniste = valintakoeTunniste;
  }

  public OsallistuminenTulosDTO getOsallistuminenTulos() {
    return osallistuminenTulos;
  }

  public void setOsallistuminenTulos(OsallistuminenTulosDTO osallistuminenTulos) {
    this.osallistuminenTulos = osallistuminenTulos;
  }

  public String getNimi() {
    return nimi;
  }

  public void setNimi(String nimi) {
    this.nimi = nimi;
  }

  public boolean isLahetetaankoKoekutsut() {
    return lahetetaankoKoekutsut;
  }

  public void setLahetetaankoKoekutsut(boolean lahetetaankoKoekutsut) {
    this.lahetetaankoKoekutsut = lahetetaankoKoekutsut;
  }

  public Integer getKutsuttavienMaara() {
    return kutsuttavienMaara;
  }

  public void setKutsuttavienMaara(final Integer kutsuttavienMaara) {
    this.kutsuttavienMaara = kutsuttavienMaara;
  }
}
