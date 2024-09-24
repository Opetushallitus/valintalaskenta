package fi.vm.sade.valinta.kooste.external.resource.tarjonta.dto;

import java.math.BigDecimal;

public class KoutaValintakoeDTO {
  private String id;
  private String tyyppi;
  private BigDecimal vahimmaispisteet;

  public String getId() {
    return id;
  }

  public void setId(String id) {
    this.id = id;
  }

  public String getTyyppi() {
    return tyyppi;
  }

  public void setTyyppi(String tyyppi) {
    this.tyyppi = tyyppi;
  }

  public BigDecimal getVahimmaispisteet() {
    return vahimmaispisteet;
  }

  public void setVahimmaispisteet(BigDecimal vahimmaispisteet) {
    this.vahimmaispisteet = vahimmaispisteet;
  }
}
