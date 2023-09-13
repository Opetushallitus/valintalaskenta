package fi.vm.sade.valintalaskenta.domain.valinta;

import java.util.*;

import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeValinnanvaihe;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.persistence.*;

@Entity(name = "Valinnanvaihe")
public class Valinnanvaihe {
  private static final Logger LOGGER = LoggerFactory.getLogger(Valinnanvaihe.class);

  @Id
  private UUID id;

  @Column
  private int jarjestysnumero;

  @Column
  private Date createdAt;

  @Column
  private String hakuOid;

  @Column
  private String hakukohdeOid;

  @Column
  private String valinnanvaiheOid;

  @Column
  private String tarjoajaOid;

  @Column
  private String nimi;

  @OneToMany(mappedBy = "valinnanvaihe")
  private List<Valintatapajono> valintatapajonot = new ArrayList<>();

  @OneToMany(mappedBy = "valinnanvaihe")
  private List<ValintakoeValinnanvaihe> valintakoeValinnanvaiheet = new ArrayList<>();

  @PrePersist
  private void prePersist() {
    createdAt = new Date();
  }

  @PostLoad
  private void jarjestaValintatapajonot() {
    Collections.sort(valintatapajonot, Comparator.comparingInt(Valintatapajono::getPrioriteetti));
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
    this.valintatapajonot = valintatapajonot;
  }

  public String getNimi() {
    return nimi;
  }

  public void setNimi(String nimi) {
    this.nimi = nimi;
  }

  public List<ValintakoeValinnanvaihe> getValintakoeValinnanvaiheet() {
    return valintakoeValinnanvaiheet;
  }

  public void setValintakoeValinnanvaiheet(List<ValintakoeValinnanvaihe> valintakoeValinnanvaiheet) {
    this.valintakoeValinnanvaiheet = valintakoeValinnanvaiheet;
  }

  public boolean hylattyValisijoittelussa(String hakemusoid) {
    return getValintatapajonot().stream()
        .flatMap(j -> j.getJonosijat().stream())
        .filter(j -> j.getHakemusOid().equals(hakemusoid))
        .anyMatch(Jonosija::isHylattyValisijoittelussa);
  }

  public void reportDuplicateValintatapajonoOids() {
    Set<String> uniqueJonoOids = new HashSet<>();
    for (Valintatapajono valintatapajono : valintatapajonot) {
      String valintatapajonoOid = valintatapajono.getValintatapajonoOid();
      if (uniqueJonoOids.contains(valintatapajonoOid)) {
        logDuplicate(valintatapajonoOid);
      }
      uniqueJonoOids.add(valintatapajonoOid);
    }
  }

  private void logDuplicate(String valintatapajonoOid) {
    LOGGER.error(
        "Warning: duplicate valintatapajonoOid"
            + valintatapajonoOid
            + " detected when saving Valinnanvaihe "
            + valinnanvaiheOid
            + " . Valintatapajonos are: "
            + valintatapajonot.stream().map(ToStringBuilder::reflectionToString),
        new RuntimeException());
  }
}
