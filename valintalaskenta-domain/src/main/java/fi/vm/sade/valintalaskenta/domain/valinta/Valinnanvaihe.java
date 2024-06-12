package fi.vm.sade.valintalaskenta.domain.valinta;

import java.util.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.PersistenceCreator;

public class Valinnanvaihe {

  @Id private UUID id;

  private int jarjestysnumero;

  private Date createdAt = new Date();

  private String hakuOid;

  private String hakukohdeOid;

  private String valinnanvaiheOid;

  private String tarjoajaOid;

  private String nimi;

  private List<Valintatapajono> valintatapajonot = new ArrayList<>();

  private Date lastModified;

  public Valinnanvaihe() {}

  @PersistenceCreator
  public Valinnanvaihe(List<Valintatapajono> valintatapajonot) {
    this.valintatapajonot.addAll(valintatapajonot);
    this.jarjestaValintatapajonot();
  }

  private void jarjestaValintatapajonot() {
    valintatapajonot.sort(Comparator.comparingInt(Valintatapajono::getPrioriteetti));
  }

  public UUID getId() {
    return id;
  }

  public void setId(UUID id) {
    this.id = id;
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

  public String getHakuOid() {
    return hakuOid;
  }

  public void setHakuOid(String hakuOid) {
    this.hakuOid = hakuOid;
  }

  public String getHakukohdeOid() {
    return hakukohdeOid;
  }

  public void setHakukohdeOid(String hakukohdeOid) {
    this.hakukohdeOid = hakukohdeOid;
  }

  public String getValinnanVaiheOid() {
    return valinnanvaiheOid;
  }

  public void setValinnanVaiheOid(String valinnanvaiheOid) {
    this.valinnanvaiheOid = valinnanvaiheOid;
  }

  public String getTarjoajaOid() {
    return tarjoajaOid;
  }

  public void setTarjoajaOid(String tarjoajaOid) {
    this.tarjoajaOid = tarjoajaOid;
  }

  public List<Valintatapajono> getValintatapajonot() {
    return valintatapajonot;
  }

  public void setValintatapajonot(List<Valintatapajono> valintatapajonot) {
    if (valintatapajonot == null) {
      this.valintatapajonot = null;
    } else {
      this.valintatapajonot.clear();
      this.valintatapajonot.addAll(valintatapajonot);
      jarjestaValintatapajonot();
    }
  }

  public String getNimi() {
    return nimi;
  }

  public void setNimi(String nimi) {
    this.nimi = nimi;
  }

  public boolean hylattyValisijoittelussa(String hakemusoid) {
    return getValintatapajonot().stream()
        .flatMap(j -> j.getJonosijat().stream())
        .filter(j -> j.getHakemusOid().equals(hakemusoid))
        .anyMatch(Jonosija::isHylattyValisijoittelussa);
  }

  public Date getLastModified() {
    return lastModified;
  }

  public void setLastModified(Date lastModified) {
    this.lastModified = lastModified;
  }
}
