package fi.vm.sade.valintalaskenta.domain.valintakoe;

import java.util.*;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.PersistenceCreator;
import org.springframework.data.annotation.Transient;
import org.springframework.data.relational.core.mapping.Column;

public class Hakutoive {

  @Id private UUID id;

  private String hakukohdeOid;

  private final Set<ValintakoeValinnanvaihe> valintakoeValinnanvaiheet = new HashSet<>();

  @Transient
  @Column("valintakoe_osallistuminen")
  private ValintakoeOsallistuminen valintakoeOsallistuminen;

  private Date createdAt;

  public Hakutoive() {}

  @PersistenceCreator
  public Hakutoive(Set<ValintakoeValinnanvaihe> valintakoeValinnanvaiheet) {
    this.valintakoeValinnanvaiheet.addAll(valintakoeValinnanvaiheet);
  }

  public String getHakukohdeOid() {
    return hakukohdeOid;
  }

  public void setHakukohdeOid(String hakukohdeOid) {
    this.hakukohdeOid = hakukohdeOid;
  }

  public UUID getId() {
    return id;
  }

  public void setId(UUID id) {
    this.id = id;
  }

  public ValintakoeOsallistuminen getValintakoeOsallistuminen() {
    return valintakoeOsallistuminen;
  }

  public void setValintakoeOsallistuminen(ValintakoeOsallistuminen valintakoeOsallistuminen) {
    this.valintakoeOsallistuminen = valintakoeOsallistuminen;
  }

  public Set<ValintakoeValinnanvaihe> getValintakoeValinnanvaiheet() {
    return valintakoeValinnanvaiheet;
  }

  public List<ValintakoeValinnanvaihe> getValintakoeValinnanvaiheetAsList() {
    return new ArrayList<>(valintakoeValinnanvaiheet);
  }

  public void setValintakoeValinnanvaiheet(Set<ValintakoeValinnanvaihe> valintakoeValinnanvaiheet) {
    this.valintakoeValinnanvaiheet.clear();
    this.valintakoeValinnanvaiheet.addAll(valintakoeValinnanvaiheet);
  }

  public Date getCreatedAt() {
    return createdAt;
  }

  public void setCreatedAt(Date createdAt) {
    this.createdAt = createdAt;
  }

  @Override
  public String toString() {
    return ToStringBuilder.reflectionToString(this);
  }
}
