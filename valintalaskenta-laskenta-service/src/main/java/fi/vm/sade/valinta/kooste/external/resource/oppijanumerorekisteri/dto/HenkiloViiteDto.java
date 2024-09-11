package fi.vm.sade.valinta.kooste.external.resource.oppijanumerorekisteri.dto;

public class HenkiloViiteDto {
  private String henkiloOid;
  private String masterOid;

  public HenkiloViiteDto(String henkiloOid, String masterOid) {
    this.henkiloOid = henkiloOid;
    this.masterOid = masterOid;
  }

  public String getHenkiloOid() {
    return henkiloOid;
  }

  public String getMasterOid() {
    return masterOid;
  }

  public void setHenkiloOid(String henkiloOid) {
    this.henkiloOid = henkiloOid;
  }

  public void setMasterOid(String masterOid) {
    this.masterOid = masterOid;
  }

  @Override
  public String toString() {
    return "henkiloOid: " + henkiloOid + ", masterOid: " + masterOid;
  }
}
