package fi.vm.sade.valintalaskenta.domain.dto.siirtotiedosto;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

public class ValintakoeOsallistuminenSiirtotiedostoDTO {
  private String hakuOid;

  private String hakemusOid;

  private String hakijaOid;

  private Date createdAt;

  private List<HakutoiveSiirtotiedostoDTO> hakutoiveet = new ArrayList<>();

  public String getHakuOid() {
    return hakuOid;
  }

  public void setHakuOid(String hakuOid) {
    this.hakuOid = hakuOid;
  }

  public String getHakemusOid() {
    return hakemusOid;
  }

  public void setHakemusOid(String hakemusOid) {
    this.hakemusOid = hakemusOid;
  }

  public String getHakijaOid() {
    return hakijaOid;
  }

  public void setHakijaOid(String hakijaOid) {
    this.hakijaOid = hakijaOid;
  }

  public Date getCreatedAt() {
    return createdAt;
  }

  public void setCreatedAt(Date createdAt) {
    this.createdAt = createdAt;
  }

  public List<HakutoiveSiirtotiedostoDTO> getHakutoiveet() {
    return hakutoiveet;
  }

  public void setHakutoiveet(List<HakutoiveSiirtotiedostoDTO> hakutoiveet) {
    this.hakutoiveet = hakutoiveet;
  }

  private String lastModified;

  public String getLastModified() {
    return lastModified;
  }

  public void setLastModified(String lastModified) {
    this.lastModified = lastModified;
  }
}
