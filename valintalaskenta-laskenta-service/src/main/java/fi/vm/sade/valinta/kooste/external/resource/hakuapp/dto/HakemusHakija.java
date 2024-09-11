package fi.vm.sade.valinta.kooste.external.resource.hakuapp.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class HakemusHakija {

  private Hakemus hakemus;
  private String opiskelijaOid;

  public void setOpiskelijaOid(String opiskelijaOid) {
    this.opiskelijaOid = opiskelijaOid;
  }

  public void setHakemus(Hakemus hakemus) {
    this.hakemus = hakemus;
  }

  public String getOpiskelijaOid() {
    return opiskelijaOid;
  }

  public Hakemus getHakemus() {
    return hakemus;
  }
}
