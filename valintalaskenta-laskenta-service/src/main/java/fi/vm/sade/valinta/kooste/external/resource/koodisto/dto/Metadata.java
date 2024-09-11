package fi.vm.sade.valinta.kooste.external.resource.koodisto.dto;

public class Metadata {
  private String nimi;
  private String kieli;

  private String kuvaus;

  public void setKieli(String kieli) {
    this.kieli = kieli;
  }

  public void setNimi(String nimi) {
    this.nimi = nimi;
  }

  public void setKuvaus(String kuvaus) {
    this.kuvaus = kuvaus;
  }

  public String getKieli() {
    return kieli;
  }

  public String getNimi() {
    return nimi;
  }

  public String getKuvaus() {
    return kuvaus;
  }
}
