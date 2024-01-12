package fi.vm.sade.valintalaskenta.domain.valinta;

import java.util.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Column;

public class MuokattuJonosija {

  @Id private UUID id;

  private String hakukohdeOid;

  private String hakuOid;

  private String valintatapajonoOid;

  private String hakemusOid;

  private Boolean harkinnanvarainen;

  private Integer prioriteetti; // hakutoive

  private String muokkaaja;

  @Column("jarjestyskriteeritulokset")
  private JarjestyskriteeritulosContainer jarjestyskriteeritulokset;

  public MuokattuJonosija() {
    jarjestyskriteeritulokset = new JarjestyskriteeritulosContainer();
  }

  public List<Jarjestyskriteeritulos> getJarjestyskriteerit() {
    return jarjestyskriteeritulokset.jarjestyskriteeritulokset;
  }

  public void setJarjestyskriteerit(Set<Jarjestyskriteeritulos> jarjestyskriteerit) {
    this.jarjestyskriteeritulokset.jarjestyskriteeritulokset.clear();
    this.jarjestyskriteeritulokset.jarjestyskriteeritulokset.addAll(jarjestyskriteerit);
    this.jarjestaJarjestyskriteeritulokset();
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

  private void jarjestaJarjestyskriteeritulokset() {
    Collections.sort(
        jarjestyskriteeritulokset.jarjestyskriteeritulokset,
        new Comparator<Jarjestyskriteeritulos>() {
          @Override
          public int compare(Jarjestyskriteeritulos o1, Jarjestyskriteeritulos o2) {
            return o1.getPrioriteetti() - o2.getPrioriteetti();
          }
        });
  }

  public String getMuokkaaja() {
    return muokkaaja;
  }

  public void setMuokkaaja(String muokkaaja) {
    this.muokkaaja = muokkaaja;
  }
}
