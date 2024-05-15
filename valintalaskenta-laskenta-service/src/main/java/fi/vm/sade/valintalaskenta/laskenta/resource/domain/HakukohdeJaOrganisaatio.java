package fi.vm.sade.valintalaskenta.laskenta.resource.domain;

public class HakukohdeJaOrganisaatio {
  private final String hakukohdeOid;
  private final String organisaatioOid;

  public HakukohdeJaOrganisaatio() {
    this.hakukohdeOid =
        "***HakukohdeJaOrganisaation hakukohdeOid asetettu ilman parametria, tämän näkyminen logilla indikoi ongelmaa***";
    this.organisaatioOid =
        "***HakukohdeJaOrganisaation organisaatioOid asetettu ilman parametria, tämän näkyminen logilla indikoi ongelmaa***";
  }

  public HakukohdeJaOrganisaatio(String hakukohdeOid, String organisaatioOid) {
    this.hakukohdeOid = hakukohdeOid;
    this.organisaatioOid = organisaatioOid;
  }

  public String getHakukohdeOid() {
    return hakukohdeOid;
  }

  public String getOrganisaatioOid() {
    return organisaatioOid;
  }
}
