package fi.vm.sade.valintalaskenta.domain.valintakoe;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.Transient;
import org.springframework.data.relational.core.mapping.Table;


@Table("ValintakoeValinnanVaihe")
public class ValintakoeValinnanvaihe {

  @Id
  private UUID id;

  @Transient
  private Valinnanvaihe valinnanvaihe;

  private Integer valinnanVaiheJarjestysluku;

  private Integer laskettavaJarjestysluku;

  @Transient
  private Hakutoive hakutoive;

  @Transient
  private List<Valintakoe> valintakokeet = new ArrayList<>();

  public Valinnanvaihe getValinnanvaihe() {
    return valinnanvaihe;
  }

  public void setValinnanvaihe(Valinnanvaihe valinnanvaihe) {
    this.valinnanvaihe = valinnanvaihe;
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
