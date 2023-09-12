package fi.vm.sade.valintalaskenta.domain.valinta;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import javax.persistence.*;

@Entity
@Table(name = "muokattujonosija")
public class MuokattuJonosija {

  @Id
  private UUID id;

  @Column
  private String hakukohdeOid;

  @Column
  private String hakuOid;

  @Column
  private String valintatapajonoOid;

  @Column
  private String hakemusOid;

  @Column
  private Integer prioriteetti; // hakutoive

  @OneToMany(mappedBy = "muokattuJonosija")
  private List<Jarjestyskriteeritulos> jarjestyskriteerit = new ArrayList<Jarjestyskriteeritulos>();

  @OneToMany(mappedBy = "muokattuJonosija")
  public List<Jarjestyskriteeritulos> getJarjestyskriteerit() {
    return jarjestyskriteerit;
  }

  public void setJarjestyskriteerit(List<Jarjestyskriteeritulos> jarjestyskriteerit) {
    this.jarjestyskriteerit = jarjestyskriteerit;
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
}
