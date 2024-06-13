package fi.vm.sade.valintalaskenta.domain.dto.siirtotiedosto;

import java.util.ArrayList;
import java.util.List;

public class ValintakoeValinnanvaiheSiirtotiedostoDTO {
  private String valinnanVaiheOid;

  private Integer valinnanVaiheJarjestysluku;

  private List<ValintakoeSiirtotiedostoDTO> valintakokeet = new ArrayList<>();

  public String getValinnanVaiheOid() {
    return valinnanVaiheOid;
  }

  public void setValinnanVaiheOid(String valinnanVaiheOid) {
    this.valinnanVaiheOid = valinnanVaiheOid;
  }

  public Integer getValinnanVaiheJarjestysluku() {
    return valinnanVaiheJarjestysluku;
  }

  public void setValinnanVaiheJarjestysluku(Integer valinnanVaiheJarjestysluku) {
    this.valinnanVaiheJarjestysluku = valinnanVaiheJarjestysluku;
  }

  public List<ValintakoeSiirtotiedostoDTO> getValintakokeet() {
    return valintakokeet;
  }

  public void setValintakokeet(List<ValintakoeSiirtotiedostoDTO> valintakokeet) {
    this.valintakokeet = valintakokeet;
  }

  private String lastModified;

  public String getLastModified() {
    return lastModified;
  }

  public void setLastModified(String lastModified) {
    this.lastModified = lastModified;
  }
}
