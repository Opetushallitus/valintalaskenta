package fi.vm.sade.valinta.kooste.valintalaskenta.actor.dto;

import org.apache.commons.lang3.builder.ToStringBuilder;

public class UuidHakukohdeJaOrganisaatio {
  private final String uuid;
  private final HakukohdeJaOrganisaatio hakukohdeJaOrganisaatio;

  public UuidHakukohdeJaOrganisaatio(String uuid, HakukohdeJaOrganisaatio hakukohdeJaOrganisaatio) {
    this.uuid = uuid;
    this.hakukohdeJaOrganisaatio = hakukohdeJaOrganisaatio;
  }

  public HakukohdeJaOrganisaatio getHakukohdeJaOrganisaatio() {
    return hakukohdeJaOrganisaatio;
  }

  public String getUuid() {
    return uuid;
  }

  @Override
  public String toString() {
    return ToStringBuilder.reflectionToString(this);
  }
}
