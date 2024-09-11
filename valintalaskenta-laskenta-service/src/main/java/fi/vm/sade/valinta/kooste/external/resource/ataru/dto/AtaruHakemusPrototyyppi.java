package fi.vm.sade.valinta.kooste.external.resource.ataru.dto;

import fi.vm.sade.valinta.kooste.erillishaku.excel.ErillishakuRivi;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Date;

public class AtaruHakemusPrototyyppi {

  private String hakukohdeOid;
  private String hakuOid;
  private String etunimi;
  private String kutsumanimi;
  private String sukunimi;
  private String henkilotunnus;
  private String sahkoposti;
  private String syntymaAika;
  private String passinNumero;
  private String kaupunkiJaMaa;
  private String syntymapaikka;
  private String idTunnus;
  private String sukupuoli;
  private String aidinkieli;
  private String asiointikieli;
  private String puhelinnumero;
  private String osoite;
  private String postinumero;
  private String postitoimipaikka;
  private String asuinmaa;
  private String kansalaisuus;
  private String kotikunta;
  private String toisenAsteenSuoritus;
  private String toisenAsteenSuoritusmaa;

  public String getHakuOid() {
    return hakuOid;
  }

  public void setHakuOid(String hakuOid) {
    this.hakuOid = hakuOid;
  }

  public String getHakukohdeOid() {
    return hakukohdeOid;
  }

  public void setHakukohdeOid(String hakukohdeOid) {
    this.hakukohdeOid = hakukohdeOid;
  }

  public String getEtunimi() {
    return etunimi;
  }

  public void setEtunimi(String etunimi) {
    this.etunimi = etunimi;
  }

  public String getSukunimi() {
    return sukunimi;
  }

  public void setSukunimi(String sukunimi) {
    this.sukunimi = sukunimi;
  }

  public String getHenkilotunnus() {
    return henkilotunnus;
  }

  public void setHenkilotunnus(String henkilotunnus) {
    this.henkilotunnus = henkilotunnus;
  }

  public String getSahkoposti() {
    return sahkoposti;
  }

  public void setSahkoposti(String sahkoposti) {
    this.sahkoposti = sahkoposti;
  }

  public String getSyntymaAika() {
    return syntymaAika;
  }

  public void setSyntymaAika(String syntymaAika) {
    this.syntymaAika = syntymaAika;
  }

  public void setSyntymaAika(Date syntymaAika) {
    this.syntymaAika = parseDate(syntymaAika);
  }

  public String getSukupuoli() {
    return sukupuoli;
  }

  public void setSukupuoli(String sukupuoli) {
    this.sukupuoli = sukupuoli;
  }

  public String getAidinkieli() {
    return aidinkieli;
  }

  public void setAidinkieli(String aidinkieli) {
    this.aidinkieli = aidinkieli;
  }

  public String getAsiointikieli() {
    return asiointikieli;
  }

  public void setAsiointikieli(String asiointikieli) {
    this.asiointikieli = asiointikieli;
  }

  public String getPuhelinnumero() {
    return puhelinnumero;
  }

  public void setPuhelinnumero(String puhelinnumero) {
    this.puhelinnumero = puhelinnumero;
  }

  public String getOsoite() {
    return osoite;
  }

  public void setOsoite(String osoite) {
    this.osoite = osoite;
  }

  public String getPostinumero() {
    return postinumero;
  }

  public void setPostinumero(String postinumero) {
    this.postinumero = postinumero;
  }

  public String getPostitoimipaikka() {
    return postitoimipaikka;
  }

  public void setPostitoimipaikka(String postitoimipaikka) {
    this.postitoimipaikka = postitoimipaikka;
  }

  public String getAsuinmaa() {
    return asuinmaa;
  }

  public void setAsuinmaa(String asuinmaa) {
    this.asuinmaa = asuinmaa;
  }

  public String getKansalaisuus() {
    return kansalaisuus;
  }

  public void setKansalaisuus(String kansalaisuus) {
    this.kansalaisuus = kansalaisuus;
  }

  public String getKotikunta() {
    return kotikunta;
  }

  public void setKotikunta(String kotikunta) {
    this.kotikunta = kotikunta;
  }

  public String getToisenAsteenSuoritus() {
    return toisenAsteenSuoritus;
  }

  // NB: This is correct: ataru form answer for this question maps YES = 0, NO = 1
  public void setToisenAsteenSuoritus(Boolean toisenAsteenSuoritus) {
    if (toisenAsteenSuoritus == null) {
      this.toisenAsteenSuoritus = "1";
    } else {
      this.toisenAsteenSuoritus = toisenAsteenSuoritus ? "0" : "1";
    }
  }

  public void setToisenAsteenSuoritus(String toisenAsteenSuoritus) {
    this.toisenAsteenSuoritus = toisenAsteenSuoritus;
  }

  public String getToisenAsteenSuoritusmaa() {
    return toisenAsteenSuoritusmaa;
  }

  public void setToisenAsteenSuoritusmaa(String toisenAsteenSuoritusmaa) {
    this.toisenAsteenSuoritusmaa = toisenAsteenSuoritusmaa;
  }

  public String getKutsumanimi() {
    return kutsumanimi;
  }

  public void setKutsumanimi(String kutsumanimi) {
    this.kutsumanimi = kutsumanimi;
  }

  public String getPassinNumero() {
    return passinNumero;
  }

  public void setPassinNumero(String passinNumero) {
    this.passinNumero = passinNumero;
  }

  public String getKaupunkiJaMaa() {
    return kaupunkiJaMaa;
  }

  public void setKaupunkiJaMaa(String kaupunkiJaMaa) {
    this.kaupunkiJaMaa = kaupunkiJaMaa;
  }

  public String getSyntymapaikka() {
    return syntymapaikka;
  }

  public void setSyntymapaikka(String syntymapaikka) {
    this.syntymapaikka = syntymapaikka;
  }

  public String getIdTunnus() {
    return idTunnus;
  }

  public void setIdTunnus(String idTunnus) {
    this.idTunnus = idTunnus;
  }

  public static String parseDate(final Date syntymaAika) {
    if (syntymaAika == null) {
      return null;
    }

    LocalDate localDate = syntymaAika.toInstant().atZone(ZoneId.systemDefault()).toLocalDate();
    return localDate.format(ErillishakuRivi.SYNTYMAAIKAFORMAT);
  }

  public static String parseDate(final LocalDate syntymaAika) {
    if (syntymaAika == null) {
      return null;
    }

    return syntymaAika.format(ErillishakuRivi.SYNTYMAAIKAFORMAT);
  }

  /**
   * @return ***HETU*** if {@link #henkilotunnus} is not empty, otherwise empty string
   */
  private String getCensoredHETUIfExists() {
    if (this.henkilotunnus != null && this.henkilotunnus.length() > 0) {
      return "***HETU***";
    } else {
      return "";
    }
  }

  @Override
  public String toString() {
    return "AtaruHakemusPrototyyppi{"
        + ", etunimi='"
        + etunimi
        + '\''
        + ", sukunimi='"
        + sukunimi
        + '\''
        + ", kutsumanimi='"
        + kutsumanimi
        + '\''
        + ", henkilotunnus='"
        + getCensoredHETUIfExists()
        + '\''
        + ", sahkoposti='"
        + sahkoposti
        + '\''
        + ", syntymaAika='"
        + syntymaAika
        + '\''
        + ", passinNumero='"
        + passinNumero
        + '\''
        + ", kaupunkiJaMaa='"
        + kaupunkiJaMaa
        + '\''
        + ", syntymapaikka='"
        + syntymapaikka
        + '\''
        + ", idTunnus='"
        + idTunnus
        + '\''
        + ", sukupuoli='"
        + sukupuoli
        + '\''
        + ", aidinkieli='"
        + aidinkieli
        + '\''
        + ", asiointikieli='"
        + asiointikieli
        + '\''
        + ", puhelinnumero='"
        + puhelinnumero
        + '\''
        + ", osoite='"
        + osoite
        + '\''
        + ", postinumero='"
        + postinumero
        + '\''
        + ", postitoimipaikka='"
        + postitoimipaikka
        + '\''
        + ", asuinmaa='"
        + asuinmaa
        + '\''
        + ", kansalaisuus='"
        + kansalaisuus
        + '\''
        + ", kotikunta='"
        + kotikunta
        + '\''
        + ", toisenAsteenSuoritus="
        + toisenAsteenSuoritus
        + ", toisenAsteenSuoritusmaa='"
        + toisenAsteenSuoritusmaa
        + '\''
        + '}';
  }
}
