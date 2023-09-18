package fi.vm.sade.valintalaskenta.domain.valintakoe;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.UUID;

import org.apache.commons.lang3.builder.ToStringBuilder;

import javax.persistence.*;

@Entity
@Table(name = "valintakoeosallistuminen")
public class ValintakoeOsallistuminen {
  @Id
  @GeneratedValue(strategy = GenerationType.AUTO)
  private UUID id;

  @Column
  private String hakuOid;

  @Column
  private String hakemusOid;

  @Column
  private String hakijaOid;

  @Column
  private String etunimi;

  @Column
  private String sukunimi;

  @Column
  private Date createdAt;

  @OneToMany(mappedBy = "valintakoeOsallistuminen")
  private List<Hakutoive> hakutoiveet = new ArrayList<>();

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

  public List<Hakutoive> getHakutoiveet() {
    return hakutoiveet;
  }

  public void setHakutoiveet(List<Hakutoive> hakutoiveet) {
    this.hakutoiveet = hakutoiveet;
  }

  @PrePersist
  private void prePersist() {
    createdAt = new Date();
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
