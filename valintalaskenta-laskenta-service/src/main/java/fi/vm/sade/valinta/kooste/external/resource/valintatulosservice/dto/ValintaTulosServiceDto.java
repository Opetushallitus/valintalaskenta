package fi.vm.sade.valinta.kooste.external.resource.valintatulosservice.dto;

import java.util.Collection;

public class ValintaTulosServiceDto {
  private String hakemusOid;
  private String hakijaOid;
  private Collection<HakutoiveDto> hakutoiveet;

  public Collection<HakutoiveDto> getHakutoiveet() {
    return hakutoiveet;
  }

  public void setHakutoiveet(Collection<HakutoiveDto> hakutoiveet) {
    this.hakutoiveet = hakutoiveet;
  }

  public void setHakemusOid(String hakemusOid) {
    this.hakemusOid = hakemusOid;
  }

  public void setHakijaOid(String hakijaOid) {
    this.hakijaOid = hakijaOid;
  }

  public String getHakemusOid() {
    return hakemusOid;
  }

  public String getHakijaOid() {
    return hakijaOid;
  }
}
