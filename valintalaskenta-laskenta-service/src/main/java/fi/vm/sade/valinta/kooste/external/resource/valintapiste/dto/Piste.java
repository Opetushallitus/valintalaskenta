package fi.vm.sade.valinta.kooste.external.resource.valintapiste.dto;

public class Piste {
  private String tunniste;
  private String arvo;
  private Osallistuminen osallistuminen;
  private String tallettaja;

  public Piste() {}

  public Piste(String tunniste, String arvo, Osallistuminen osallistuminen, String tallettaja) {
    this.tunniste = tunniste;
    this.arvo = arvo;
    this.osallistuminen = osallistuminen;
    this.tallettaja = tallettaja;
  }

  public Osallistuminen getOsallistuminen() {
    return osallistuminen;
  }

  public String getArvo() {
    return arvo;
  }

  public String getTallettaja() {
    return tallettaja;
  }

  public String getTunniste() {
    return tunniste;
  }
}
