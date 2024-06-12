package fi.vm.sade.valintalaskenta.domain.dto.siirtotiedosto;

import java.util.ArrayList;
import java.util.List;

public class HakutoiveSiirtotiedostoDTO {
  private String hakukohdeOid;

  private List<ValintakoeValinnanvaiheSiirtotiedostoDTO> valinnanVaiheet = new ArrayList<>();

  public String getHakukohdeOid() {
    return hakukohdeOid;
  }

  public void setHakukohdeOid(String hakukohdeOid) {
    this.hakukohdeOid = hakukohdeOid;
  }

  public List<ValintakoeValinnanvaiheSiirtotiedostoDTO> getValinnanVaiheet() {
    return valinnanVaiheet;
  }

  public void setValinnanVaiheet(List<ValintakoeValinnanvaiheSiirtotiedostoDTO> valinnanVaiheet) {
    this.valinnanVaiheet = valinnanVaiheet;
  }

  private String lastModified;

  public String getLastModified() {
    return lastModified;
  }

  public void setLastModified(String lastModified) {
    this.lastModified = lastModified;
  }
}
