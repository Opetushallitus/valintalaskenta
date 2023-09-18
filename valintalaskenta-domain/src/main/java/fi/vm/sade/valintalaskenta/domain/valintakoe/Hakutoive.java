package fi.vm.sade.valintalaskenta.domain.valintakoe;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import org.apache.commons.lang3.builder.ToStringBuilder;

import javax.persistence.*;

@Entity(name = "Hakutoive")
public class Hakutoive {

  @Id
  @GeneratedValue(strategy = GenerationType.AUTO)
  private UUID id;

  @Column
  private String hakukohdeOid;

  @Column
  private String laskettavaHakukohdeOid;

  @OneToMany(mappedBy = "hakutoive")
  private List<ValintakoeValinnanvaihe> valintakoeValinnanvaiheet;

  @ManyToOne
  private ValintakoeOsallistuminen valintakoeOsallistuminen;

  public String getHakukohdeOid() {
    return hakukohdeOid;
  }

  public void setHakukohdeOid(String hakukohdeOid) {
    this.hakukohdeOid = hakukohdeOid;
  }

  public String getLaskettavaHakukohdeOid() {
    return laskettavaHakukohdeOid;
  }

  public void setLaskettavaHakukohdeOid(String laskettavaHakukohdeOid) {
    this.laskettavaHakukohdeOid = laskettavaHakukohdeOid;
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

  public List<ValintakoeValinnanvaihe> getValintakoeValinnanvaiheet() {
    return valintakoeValinnanvaiheet;
  }

  public void setValintakoeValinnanvaiheet(List<ValintakoeValinnanvaihe> valintakoeValinnanvaiheet) {
    this.valintakoeValinnanvaiheet = valintakoeValinnanvaiheet;
  }

  @Override
  public String toString() {
    return ToStringBuilder.reflectionToString(this);
  }
}
