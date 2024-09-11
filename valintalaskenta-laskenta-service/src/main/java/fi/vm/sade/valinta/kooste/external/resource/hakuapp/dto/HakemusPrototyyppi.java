package fi.vm.sade.valinta.kooste.external.resource.hakuapp.dto;

import fi.vm.sade.valinta.kooste.erillishaku.excel.ErillishakuRivi;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Date;

public class HakemusPrototyyppi {

  private String hakijaOid;
  private String etunimi;
  private String sukunimi;
  private String henkilotunnus;
  private String sahkoposti;
  private String syntymaAika;
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
  private Boolean toisenAsteenSuoritus;
  private String toisenAsteenSuoritusmaa;
  private String maksuvelvollisuus;

  public String getHakijaOid() {
    return hakijaOid;
  }

  public void setHakijaOid(String hakijaOid) {
    this.hakijaOid = hakijaOid;
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

  public Boolean getToisenAsteenSuoritus() {
    return toisenAsteenSuoritus;
  }

  public void setToisenAsteenSuoritus(Boolean toisenAsteenSuoritus) {
    this.toisenAsteenSuoritus = toisenAsteenSuoritus;
  }

  public String getToisenAsteenSuoritusmaa() {
    return toisenAsteenSuoritusmaa;
  }

  public void setToisenAsteenSuoritusmaa(String toisenAsteenSuoritusmaa) {
    this.toisenAsteenSuoritusmaa = toisenAsteenSuoritusmaa;
  }

  public String getMaksuvelvollisuus() {
    return maksuvelvollisuus;
  }

  public void setMaksuvelvollisuus(String maksuvelvollisuus) {
    this.maksuvelvollisuus = maksuvelvollisuus;
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
    return "HakemusPrototyyppi{"
        + "hakijaOid='"
        + hakijaOid
        + '\''
        + ", etunimi='"
        + etunimi
        + '\''
        + ", sukunimi='"
        + sukunimi
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
        + ", maksuvelvollisuus='"
        + maksuvelvollisuus
        + '\''
        + '}';
  }
}
