package fi.vm.sade.valintalaskenta.domain.valintakoe;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.springframework.data.annotation.Id;


public class ValintakoeValinnanvaihe {

  @Id
  private UUID id;

  private Valinnanvaihe valinnanVaihe;

  private Integer valinnanVaiheJarjestysluku;

  private Integer laskettavaJarjestysluku;

  private Hakutoive hakutoive;

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
