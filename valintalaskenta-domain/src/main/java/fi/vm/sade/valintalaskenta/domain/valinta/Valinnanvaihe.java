package fi.vm.sade.valintalaskenta.domain.valinta;

import java.util.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.PersistenceCreator;

public class Valinnanvaihe {
  private static final Logger LOGGER = LoggerFactory.getLogger(Valinnanvaihe.class);

  @Id private UUID id;

  private int jarjestysnumero;

  private Date createdAt;

  private String hakuOid;

  private String hakukohdeOid;

  private String valinnanvaiheOid;

  private String tarjoajaOid;

  private String nimi;

  private final List<Valintatapajono> valintatapajono = new ArrayList<>();

  public Valinnanvaihe() {}

  @PersistenceCreator
  public Valinnanvaihe(List<Valintatapajono> valintatapajono) {
    this.valintatapajono.addAll(valintatapajono);
    this.jarjestaValintatapajonot();
  }

  private void jarjestaValintatapajonot() {
    valintatapajono.sort(Comparator.comparingInt(Valintatapajono::getPrioriteetti));
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

  public List<Valintatapajono> getValintatapajono() {
    return valintatapajono;
  }

  public void setValintatapajono(List<Valintatapajono> valintatapajono) {
    this.valintatapajono.clear();
    this.valintatapajono.addAll(valintatapajono);
    jarjestaValintatapajonot();
  }

  public String getNimi() {
    return nimi;
  }

  public void setNimi(String nimi) {
    this.nimi = nimi;
  }

  public boolean hylattyValisijoittelussa(String hakemusoid) {
    return getValintatapajono().stream()
        .flatMap(j -> j.getJonosijat().stream())
        .filter(j -> j.getHakemusOid().equals(hakemusoid))
        .anyMatch(Jonosija::isHylattyValisijoittelussa);
  }
}
