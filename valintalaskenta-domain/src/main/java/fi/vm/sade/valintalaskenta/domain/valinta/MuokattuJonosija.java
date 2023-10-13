package fi.vm.sade.valintalaskenta.domain.valinta;

import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.PersistenceCreator;

import java.util.*;


public class MuokattuJonosija {

  @Id
  private UUID id;

  private String hakukohdeOid;

  private String hakuOid;

  private String valintatapajonoOid;

  private String hakemusOid;

  private Boolean harkinnanvarainen;

  private Integer prioriteetti; // hakutoive

  private final Set<Jarjestyskriteeritulos> jarjestyskriteerit = new HashSet<>();

  public MuokattuJonosija() {}

  @PersistenceCreator
  public MuokattuJonosija(Set<Jarjestyskriteeritulos> jarjestyskriteerit) {
    this.jarjestyskriteerit.addAll(jarjestyskriteerit);
  }

  public Set<Jarjestyskriteeritulos> getJarjestyskriteerit() {
    return jarjestyskriteerit;
  }

  public void setJarjestyskriteerit(Set<Jarjestyskriteeritulos> jarjestyskriteerit) {
    this.jarjestyskriteerit.clear();
    this.jarjestyskriteerit.addAll(jarjestyskriteerit);
  }

  public String getValintatapajonoOid() {
    return valintatapajonoOid;
  }

  public void setValintatapajonoOid(String valintatapajonoOid) {
    this.valintatapajonoOid = valintatapajonoOid;
  }

  public String getHakemusOid() {
    return hakemusOid;
  }

  public void setHakemusOid(String hakemusOid) {
    this.hakemusOid = hakemusOid;
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

  public Integer getPrioriteetti() {
    return prioriteetti;
  }

  public void setPrioriteetti(Integer prioriteetti) {
    this.prioriteetti = prioriteetti;
  }

  public UUID getId() {
    return id;
  }

  public void setId(UUID id) {
    this.id = id;
  }

  public Boolean getHarkinnanvarainen() {
    return harkinnanvarainen;
  }

  public void setHarkinnanvarainen(Boolean harkinnanvarainen) {
    this.harkinnanvarainen = harkinnanvarainen;
  }
}
