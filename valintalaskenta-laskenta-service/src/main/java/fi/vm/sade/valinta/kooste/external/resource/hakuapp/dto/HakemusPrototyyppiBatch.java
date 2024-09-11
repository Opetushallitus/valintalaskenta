package fi.vm.sade.valinta.kooste.external.resource.hakuapp.dto;

import java.util.Collection;

public class HakemusPrototyyppiBatch {
  private String hakuOid;
  private String hakukohdeOid;
  private String tarjoajaOid;
  private Collection<HakemusPrototyyppi> hakemukset;

  public HakemusPrototyyppiBatch(
      String hakuOid,
      String hakukohdeOid,
      String tarjoajaOid,
      Collection<HakemusPrototyyppi> hakemusPrototyypit) {
    this.hakuOid = hakuOid;
    this.hakukohdeOid = hakukohdeOid;
    this.tarjoajaOid = tarjoajaOid;
    this.hakemukset = hakemusPrototyypit;
  }

  public void setHakemukset(Collection<HakemusPrototyyppi> hakemukset) {
    this.hakemukset = hakemukset;
  }

  public void setHakukohdeOid(String hakukohdeOid) {
    this.hakukohdeOid = hakukohdeOid;
  }

  public void setHakuOid(String hakuOid) {
    this.hakuOid = hakuOid;
  }

  public void setTarjoajaOid(String tarjoajaOid) {
    this.tarjoajaOid = tarjoajaOid;
  }

  public Collection<HakemusPrototyyppi> getHakemukset() {
    return hakemukset;
  }

  public String getHakukohdeOid() {
    return hakukohdeOid;
  }

  public String getHakuOid() {
    return hakuOid;
  }

  public String getTarjoajaOid() {
    return tarjoajaOid;
  }
}
