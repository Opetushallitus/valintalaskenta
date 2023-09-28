package fi.vm.sade.valintalaskenta.domain.valinta;

import java.util.*;

import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeValinnanvaihe;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.PersistenceCreator;
import org.springframework.data.annotation.Transient;


public class Valinnanvaihe {
  private static final Logger LOGGER = LoggerFactory.getLogger(Valinnanvaihe.class);

  @Id
  private UUID id;

  private int jarjestysnumero;

  private Date createdAt;

  private String hakuOid;

  private String hakukohdeOid;

  private String valinnanvaiheOid;

  private String tarjoajaOid;

  private String nimi;

  public final List<Valintatapajono> valintatapajono = new ArrayList<>();

  @Transient
  public final Set<ValintakoeValinnanvaihe> valintakoevalinnanvaihe = new HashSet<>();

  public Valinnanvaihe(){}

  @PersistenceCreator
  public Valinnanvaihe(List<Valintatapajono> valintatapajono) {
    this.valintatapajono.addAll(valintatapajono);
    //this.valintakoevalinnanvaihe.addAll(valintakoevalinnanvaihe);
  }

  private void jarjestaValintatapajonot() {
    Collections.sort(valintatapajono, Comparator.comparingInt(Valintatapajono::getPrioriteetti));
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

  public void reportDuplicateValintatapajonoOids() {
    Set<String> uniqueJonoOids = new HashSet<>();
    for (Valintatapajono valintatapajono : valintatapajono) {
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
            + valintatapajono.stream().map(ToStringBuilder::reflectionToString),
        new RuntimeException());
  }
}
