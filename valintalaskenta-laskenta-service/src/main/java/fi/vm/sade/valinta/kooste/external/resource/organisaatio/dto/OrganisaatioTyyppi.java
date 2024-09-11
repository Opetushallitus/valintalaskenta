package fi.vm.sade.valinta.kooste.external.resource.organisaatio.dto;

import java.util.List;
import java.util.Map;

public class OrganisaatioTyyppi {
  private String oid;
  private Map<String, String> nimi;
  private List<OrganisaatioTyyppi> children;
  private String oppilaitostyyppi;
  private List<String> organisaatiotyypit;

  public OrganisaatioTyyppi() {}

  public OrganisaatioTyyppi(
      String oid,
      Map<String, String> nimi,
      List<OrganisaatioTyyppi> children,
      String oppilaitostyyppi,
      List<String> organisaatiotyypit) {
    this.oid = oid;
    this.nimi = nimi;
    this.children = children;
    this.oppilaitostyyppi = oppilaitostyyppi;
    this.organisaatiotyypit = organisaatiotyypit;
  }

  public String getOid() {
    return oid;
  }

  public void setOid(String oid) {
    this.oid = oid;
  }

  public Map<String, String> getNimi() {
    return nimi;
  }

  public void setNimi(Map<String, String> nimi) {
    this.nimi = nimi;
  }

  public List<OrganisaatioTyyppi> getChildren() {
    return children;
  }

  public void setChildren(List<OrganisaatioTyyppi> children) {
    this.children = children;
  }

  public String getOppilaitostyyppi() {
    return oppilaitostyyppi;
  }

  public void setOppilaitostyyppi(String oppilaitostyyppi) {
    this.oppilaitostyyppi = oppilaitostyyppi;
  }

  public List<String> getOrganisaatiotyypit() {
    return organisaatiotyypit;
  }

  public void setOrganisaatiotyypit(List<String> organisaatiotyypit) {
    this.organisaatiotyypit = organisaatiotyypit;
  }

  @Override
  public String toString() {
    return "OrganisaatioTyyppi{"
        + "oid='"
        + oid
        + '\''
        + ", nimi="
        + nimi
        + ", children="
        + children
        + ", oppilaitostyyppi='"
        + oppilaitostyyppi
        + '\''
        + ", organisaatiotyypit="
        + organisaatiotyypit
        + '}';
  }
}
