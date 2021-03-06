package fi.vm.sade.valintalaskenta.domain.valinta;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.bson.types.ObjectId;
import org.mongodb.morphia.annotations.Entity;
import org.mongodb.morphia.annotations.Field;
import org.mongodb.morphia.annotations.Id;
import org.mongodb.morphia.annotations.Index;
import org.mongodb.morphia.annotations.IndexOptions;
import org.mongodb.morphia.annotations.Indexed;
import org.mongodb.morphia.annotations.Indexes;
import org.mongodb.morphia.annotations.PostLoad;
import org.mongodb.morphia.annotations.PrePersist;
import org.mongodb.morphia.annotations.Reference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Entity(value = "Valinnanvaihe", noClassnameStored = true)
@Indexes(
    @Index(
        fields = {@Field("hakuOid"), @Field("valinnanvaiheOid")},
        options = @IndexOptions(name = "idx_hakuoid_valinnanvaihe_oid", unique = true)))
public class Valinnanvaihe {
  private static final Logger LOGGER = LoggerFactory.getLogger(Valinnanvaihe.class);

  @Id private ObjectId id;

  private int jarjestysnumero;

  private Date createdAt;

  @Indexed(unique = false, dropDups = false)
  private String hakuOid;

  @Indexed(unique = false, dropDups = false)
  private String hakukohdeOid;

  @Indexed(unique = false, dropDups = false)
  private String valinnanvaiheOid;

  private String tarjoajaOid;

  private String nimi;

  @Reference private List<Valintatapajono> valintatapajonot = new ArrayList<>();

  @PrePersist
  private void prePersist() {
    createdAt = new Date();
  }

  @PostLoad
  private void jarjestaValintatapajonot() {
    Collections.sort(valintatapajonot, Comparator.comparingInt(Valintatapajono::getPrioriteetti));
  }

  public void setId(ObjectId id) {
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

  public String getValinnanvaiheOid() {
    return valinnanvaiheOid;
  }

  public void setValinnanvaiheOid(String valinnanvaiheOid) {
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
