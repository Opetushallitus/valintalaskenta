package fi.vm.sade.valintalaskenta.domain.dto;

public class JonoDto {
  private final String hakukohdeOid;
  private final String valintatapajonoOid;
  private final boolean valmisSijoiteltavaksi;
  private final boolean siirretaanSijoitteluun;

  public JonoDto() {
    this.hakukohdeOid = "";
    this.valintatapajonoOid = "";
    this.valmisSijoiteltavaksi = false;
    this.siirretaanSijoitteluun = false;
  }

  public JonoDto(
      String hakukohdeOid,
      String valintatapajonoOid,
      boolean valmisSijoiteltavaksi,
      boolean siirretaanSijoitteluun) {
    this.hakukohdeOid = hakukohdeOid;
    this.valintatapajonoOid = valintatapajonoOid;
    this.valmisSijoiteltavaksi = valmisSijoiteltavaksi;
    this.siirretaanSijoitteluun = siirretaanSijoitteluun;
  }

  public String getHakukohdeOid() {
    return hakukohdeOid;
  }

  public String getValintatapajonoOid() {
    return valintatapajonoOid;
  }

  public boolean isSiirretaanSijoitteluun() {
    return siirretaanSijoitteluun;
  }

  public boolean isValmisSijoiteltavaksi() {
    return valmisSijoiteltavaksi;
  }

  public boolean getSiirretaanSijoitteluun() {
    return siirretaanSijoitteluun;
  }

  public boolean getValmisSijoiteltavaksi() {
    return valmisSijoiteltavaksi;
  }
}
