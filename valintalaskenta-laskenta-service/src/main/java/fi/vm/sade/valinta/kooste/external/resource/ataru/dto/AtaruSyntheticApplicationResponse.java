package fi.vm.sade.valinta.kooste.external.resource.ataru.dto;

public class AtaruSyntheticApplicationResponse {
  private String personOid;
  private String hakemusOid;

  public AtaruSyntheticApplicationResponse() {}

  public AtaruSyntheticApplicationResponse(String personOid, String hakemusOid) {
    this.personOid = personOid;
    this.hakemusOid = hakemusOid;
  }

  public String getPersonOid() {
    return personOid;
  }

  public void setPersonOid(String personOid) {
    this.personOid = personOid;
  }

  public String getHakemusOid() {
    return hakemusOid;
  }

  public void setHakemusOid(String hakemusOid) {
    this.hakemusOid = hakemusOid;
  }
}
