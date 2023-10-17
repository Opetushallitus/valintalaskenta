package fi.vm.sade.valintalaskenta.domain.valintakoe;

import java.util.*;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.PersistenceCreator;
import org.springframework.data.relational.core.mapping.Table;

@Table("valintakoe_osallistuminen")
public class ValintakoeOsallistuminen {
  @Id private UUID id;

  private String hakuOid;

  private String hakemusOid;

  private String hakijaOid;

  private String etunimi;

  private String sukunimi;

  private Date createdAt;

  private final Set<Hakutoive> hakutoiveet = new HashSet<>();

  public ValintakoeOsallistuminen() {}

  @PersistenceCreator
  public ValintakoeOsallistuminen(Set<Hakutoive> hakutoiveet) {
    this.hakutoiveet.addAll(hakutoiveet);
  }

  public UUID getId() {
    return id;
  }

  public void setId(UUID id) {
    this.id = id;
  }

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

  public Set<Hakutoive> getHakutoiveet() {
    return hakutoiveet;
  }

  public List<Hakutoive> getHakutoiveetAsList() {
    return new ArrayList<>(hakutoiveet);
  }

  public void setHakutoiveet(Set<Hakutoive> hakutoiveet) {
    this.hakutoiveet.clear();
    this.hakutoiveet.addAll(hakutoiveet);
  }

  public Date getCreatedAt() {
    return createdAt;
  }

  public void setCreatedAt(Date createdAt) {
    this.createdAt = createdAt;
  }

  public String getSukunimi() {
    return sukunimi;
  }

  public void setSukunimi(String sukunimi) {
    this.sukunimi = sukunimi;
  }

  public String getEtunimi() {
    return etunimi;
  }

  public void setEtunimi(String etunimi) {
    this.etunimi = etunimi;
  }

  @Override
  public String toString() {
    return ToStringBuilder.reflectionToString(this);
  }
}
