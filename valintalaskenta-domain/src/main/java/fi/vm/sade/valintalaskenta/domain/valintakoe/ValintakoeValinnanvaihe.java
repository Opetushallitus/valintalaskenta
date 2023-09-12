package fi.vm.sade.valintalaskenta.domain.valintakoe;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import org.apache.commons.lang3.builder.ToStringBuilder;

import javax.persistence.*;

@Entity(name = "ValintakoeValinnanvaihe")
public class ValintakoeValinnanvaihe {

  @Id
  private UUID id;

  @ManyToOne
  private Valinnanvaihe valinnanVaihe;

  @Column
  private Integer valinnanVaiheJarjestysluku;

  @Column
  private Integer laskettavaJarjestysluku;

  @ManyToOne
  private Hakutoive hakutoive;

  @OneToMany(mappedBy = "valintakoeValinnanvaihe")
  private List<Valintakoe> valintakokeet = new ArrayList<>();

  public Valinnanvaihe getValinnanVaihe() {
    return valinnanVaihe;
  }

  public void setValinnanVaihe(Valinnanvaihe valinnanVaihe) {
    this.valinnanVaihe = valinnanVaihe;
  }

  public Integer getValinnanVaiheJarjestysluku() {
    return valinnanVaiheJarjestysluku;
  }

  public void setValinnanVaiheJarjestysluku(Integer valinnanVaiheJarjestysluku) {
    this.valinnanVaiheJarjestysluku = valinnanVaiheJarjestysluku;
  }

  public List<Valintakoe> getValintakokeet() {
    return valintakokeet;
  }

  public void setValintakokeet(List<Valintakoe> valintakokeet) {
    this.valintakokeet = valintakokeet;
  }

  public Integer getLaskettavaJarjestysluku() {
    return laskettavaJarjestysluku;
  }

  public void setLaskettavaJarjestysluku(Integer laskettavaJarjestysluku) {
    this.laskettavaJarjestysluku = laskettavaJarjestysluku;
  }

  @Override
  public String toString() {
    return ToStringBuilder.reflectionToString(this);
  }
}
