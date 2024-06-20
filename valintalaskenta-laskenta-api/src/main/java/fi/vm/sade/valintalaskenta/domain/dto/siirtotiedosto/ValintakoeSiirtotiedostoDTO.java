package fi.vm.sade.valintalaskenta.domain.dto.siirtotiedosto;

import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.OsallistuminenTulosDTO;

public class ValintakoeSiirtotiedostoDTO {
  private String valintakoeOid;

  private String valintakoeTunniste;

  private String nimi;

  private boolean aktiivinen;

  private OsallistuminenTulosDTO osallistuminenTulos;

  private boolean lahetetaankoKoekutsut;

  private Boolean kutsutaankoKaikki;

  private Integer kutsuttavienMaara;

  public String getValintakoeOid() {
    return valintakoeOid;
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

  public String getNimi() {
    return nimi;
  }

  public void setNimi(String nimi) {
    this.nimi = nimi;
  }

  public boolean isAktiivinen() {
    return aktiivinen;
  }

  public void setAktiivinen(boolean aktiivinen) {
    this.aktiivinen = aktiivinen;
  }

  public OsallistuminenTulosDTO getOsallistuminenTulos() {
    return osallistuminenTulos;
  }

  public void setOsallistuminenTulos(OsallistuminenTulosDTO osallistuminenTulos) {
    this.osallistuminenTulos = osallistuminenTulos;
  }

  public boolean isLahetetaankoKoekutsut() {
    return lahetetaankoKoekutsut;
  }

  public void setLahetetaankoKoekutsut(boolean lahetetaankoKoekutsut) {
    this.lahetetaankoKoekutsut = lahetetaankoKoekutsut;
  }

  public Boolean getKutsutaankoKaikki() {
    return kutsutaankoKaikki;
  }

  public void setKutsutaankoKaikki(Boolean kutsutaankoKaikki) {
    this.kutsutaankoKaikki = kutsutaankoKaikki;
  }

  public Integer getKutsuttavienMaara() {
    return kutsuttavienMaara;
  }

  public void setKutsuttavienMaara(Integer kutsuttavienMaara) {
    this.kutsuttavienMaara = kutsuttavienMaara;
  }

  private String lastModified;

  public String getLastModified() {
    return lastModified;
  }

  public void setLastModified(String lastModified) {
    this.lastModified = lastModified;
  }
}
