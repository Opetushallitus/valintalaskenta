package fi.vm.sade.valinta.kooste.valintalaskenta.actor.dto;

import org.apache.commons.lang3.builder.ToStringBuilder;

public class HakukohdeJaOrganisaatio {
  private String hakukohdeOid;
  private String organisaatioOid;

  public HakukohdeJaOrganisaatio() {
    this.hakukohdeOid = "";
    this.organisaatioOid = "";
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

  @Override
  public String toString() {
    return ToStringBuilder.reflectionToString(this);
  }
}
