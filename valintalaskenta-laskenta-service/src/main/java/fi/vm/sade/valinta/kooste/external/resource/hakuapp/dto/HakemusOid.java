package fi.vm.sade.valinta.kooste.external.resource.hakuapp.dto;

import com.fasterxml.jackson.annotation.JsonCreator;

public class HakemusOid {
  private final String oid;

  @JsonCreator
  public HakemusOid(String oid) {
    this.oid = oid;
  }

  public String getOid() {
    return oid;
  }
}
