package fi.vm.sade.valintalaskenta.laskenta.service.valintakoe.impl.util;

import fi.vm.sade.service.valintaperusteet.dto.model.Koekutsu;
import fi.vm.sade.valintalaskenta.domain.valintakoe.OsallistuminenTulos;

/** User: wuoti Date: 3.5.2013 Time: 9.35 */
public class HakukohdeValintakoeData {

  private String hakuOid;
  private String laskettavaHakukohdeOid;
  private String hakukohdeOid;
  private String valinnanVaiheOid;
  private int valinnanVaiheJarjestysNro;
  private int laskettavaValinnanVaiheJarjestysNro;

  private String valintakoeTunniste;
  private String valintakoeOid;
  private String nimi;
  private OsallistuminenTulos osallistuminenTulos;
  private boolean lahetetaankoKoekutsut;
  private boolean aktiivinen;
  private Integer kutsuttavienMaara;
  private Koekutsu kutsunKohde;
  private String kutsunKohdeAvain;

  public String getNimi() {
    return nimi;
  }

  public void setNimi(String nimi) {
    this.nimi = nimi;
  }

  public String getHakuOid() {
    return hakuOid;
  }

  public void setHakuOid(String hakuOid) {
    this.hakuOid = hakuOid;
  }

  public String getValintakoeTunniste() {
    return valintakoeTunniste;
  }

  public void setValintakoeTunniste(String valintakoeTunniste) {
    this.valintakoeTunniste = valintakoeTunniste;
  }

  public String getHakukohdeOid() {
    return hakukohdeOid;
  }

  public void setHakukohdeOid(String hakukohdeOid) {
    this.hakukohdeOid = hakukohdeOid;
  }

  public String getValinnanVaiheOid() {
    return valinnanVaiheOid;
  }

  public void setValinnanVaiheOid(String valinnanVaiheOid) {
    this.valinnanVaiheOid = valinnanVaiheOid;
  }

  public int getValinnanVaiheJarjestysNro() {
    return valinnanVaiheJarjestysNro;
  }

  public void setValinnanVaiheJarjestysNro(int valinnanVaiheJarjestysNro) {
    this.valinnanVaiheJarjestysNro = valinnanVaiheJarjestysNro;
  }

  public void setValintakoeOid(String valintakoeOid) {
    this.valintakoeOid = valintakoeOid;
  }

  public String getValintakoeOid() {
    return valintakoeOid;
  }

  public OsallistuminenTulos getOsallistuminenTulos() {
    return osallistuminenTulos;
  }

  public void setOsallistuminenTulos(OsallistuminenTulos osallistuminenTulos) {
    this.osallistuminenTulos = osallistuminenTulos;
  }

  public boolean isLahetetaankoKoekutsut() {
    return lahetetaankoKoekutsut;
  }

  public void setLahetetaankoKoekutsut(boolean lahetetaankoKoekutsut) {
    this.lahetetaankoKoekutsut = lahetetaankoKoekutsut;
  }

  public boolean isAktiivinen() {
    return aktiivinen;
  }

  public void setAktiivinen(boolean aktiivinen) {
    this.aktiivinen = aktiivinen;
  }

  public Integer getKutsuttavienMaara() {
    return kutsuttavienMaara;
  }

  public void setKutsuttavienMaara(final Integer kutsuttavienMaara) {
    this.kutsuttavienMaara = kutsuttavienMaara;
  }

  public Koekutsu getKutsunKohde() {
    return kutsunKohde;
  }

  public void setKutsunKohde(Koekutsu kutsunKohde) {
    this.kutsunKohde = kutsunKohde;
  }

  public String getKutsunKohdeAvain() {
    return kutsunKohdeAvain;
  }

  public void setKutsunKohdeAvain(String kutsunKohdeAvain) {
    this.kutsunKohdeAvain = kutsunKohdeAvain;
  }

  public String getLaskettavaHakukohdeOid() {
    return laskettavaHakukohdeOid;
  }

  public void setLaskettavaHakukohdeOid(String laskettavaHakukohdeOid) {
    this.laskettavaHakukohdeOid = laskettavaHakukohdeOid;
  }

  public int getLaskettavaValinnanVaiheJarjestysNro() {
    return laskettavaValinnanVaiheJarjestysNro;
  }

  public void setLaskettavaValinnanVaiheJarjestysNro(int laskettavaValinnanVaiheJarjestysNro) {
    this.laskettavaValinnanVaiheJarjestysNro = laskettavaValinnanVaiheJarjestysNro;
  }
}
