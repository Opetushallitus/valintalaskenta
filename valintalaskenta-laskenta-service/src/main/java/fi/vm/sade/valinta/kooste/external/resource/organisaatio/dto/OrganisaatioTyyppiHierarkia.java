package fi.vm.sade.valinta.kooste.external.resource.organisaatio.dto;

import java.util.List;

public class OrganisaatioTyyppiHierarkia {
  private int numHits;
  private List<OrganisaatioTyyppi> organisaatiot;

  public OrganisaatioTyyppiHierarkia() {}

  public OrganisaatioTyyppiHierarkia(int numHits, List<OrganisaatioTyyppi> organisaatiot) {
    this.numHits = numHits;
    this.organisaatiot = organisaatiot;
  }

  public int getNumHits() {
    return numHits;
  }

  public void setNumHits(int numHits) {
    this.numHits = numHits;
  }

  public List<OrganisaatioTyyppi> getOrganisaatiot() {
    return organisaatiot;
  }

  public void setOrganisaatiot(List<OrganisaatioTyyppi> organisaatiot) {
    this.organisaatiot = organisaatiot;
  }

  @Override
  public String toString() {
    return "OrganisaatioTyyppiHierarkia{"
        + "numHits="
        + numHits
        + ", organisaatiot="
        + organisaatiot
        + '}';
  }
}
