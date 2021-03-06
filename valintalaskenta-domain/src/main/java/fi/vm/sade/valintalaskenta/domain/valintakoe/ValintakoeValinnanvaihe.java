package fi.vm.sade.valintalaskenta.domain.valintakoe;

import java.util.ArrayList;
import java.util.List;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.mongodb.morphia.annotations.Embedded;

@Embedded
public class ValintakoeValinnanvaihe {
  private String valinnanVaiheOid;
  private Integer valinnanVaiheJarjestysluku;
  private Integer laskettavaJarjestysluku;

  @Embedded private List<Valintakoe> valintakokeet = new ArrayList<>();

  public String getValinnanVaiheOid() {
    return valinnanVaiheOid;
  }

  public void setValinnanVaiheOid(String valinnanVaiheOid) {
    this.valinnanVaiheOid = valinnanVaiheOid;
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
