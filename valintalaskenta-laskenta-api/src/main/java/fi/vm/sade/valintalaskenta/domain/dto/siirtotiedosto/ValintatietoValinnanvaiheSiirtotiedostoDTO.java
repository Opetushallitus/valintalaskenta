package fi.vm.sade.valintalaskenta.domain.dto.siirtotiedosto;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

public class ValintatietoValinnanvaiheSiirtotiedostoDTO {
  private int jarjestysnumero;
  private String valinnanvaiheoid;
  private String hakuOid;
  private String nimi;
  private Date createdAt;
  private String hakukohdeOid;

  private List<ValintatietoValintatapajonoSiirtotiedostoDTO> valintatapajonot = new ArrayList<>();

  public String getValinnanvaiheoid() {
    return valinnanvaiheoid;
  }

  public void setValinnanvaiheoid(String valinnanvaiheoid) {
    this.valinnanvaiheoid = valinnanvaiheoid;
  }

  public int getJarjestysnumero() {
    return jarjestysnumero;
  }

  public void setJarjestysnumero(int jarjestysnumero) {
    this.jarjestysnumero = jarjestysnumero;
  }

  public Date getCreatedAt() {
    return createdAt;
  }

  public void setCreatedAt(Date createdAt) {
    this.createdAt = createdAt;
  }

  public String getNimi() {
    return nimi;
  }

  public void setNimi(String nimi) {
    this.nimi = nimi;
  }

  public String getHakuOid() {
    return hakuOid;
  }

  public void setHakuOid(String hakuOid) {
    this.hakuOid = hakuOid;
  }

  public List<ValintatietoValintatapajonoSiirtotiedostoDTO> getValintatapajonot() {
    return valintatapajonot;
  }

  public void setValintatapajonot(
      List<ValintatietoValintatapajonoSiirtotiedostoDTO> valintatapajonot) {
    this.valintatapajonot = valintatapajonot;
  }

  private String lastModified;

  public String getLastModified() {
    return lastModified;
  }

  public void setLastModified(String lastModified) {
    this.lastModified = lastModified;
  }

  public String getHakukohdeOid() {
    return hakukohdeOid;
  }

  public void setHakukohdeOid(String hakukohdeOid) {
    this.hakukohdeOid = hakukohdeOid;
  }
}
