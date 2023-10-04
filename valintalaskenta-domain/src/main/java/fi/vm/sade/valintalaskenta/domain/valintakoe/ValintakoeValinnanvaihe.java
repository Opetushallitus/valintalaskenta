package fi.vm.sade.valintalaskenta.domain.valintakoe;

import java.util.*;

import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.PersistenceCreator;
import org.springframework.data.annotation.Transient;

public class ValintakoeValinnanvaihe {

  @Id
  private UUID id;

  private String valinnanvaiheOid;

  private Integer valinnanVaiheJarjestysluku;

  @Transient
  private Hakutoive hakutoive;

  private final Set<Valintakoe> valintakokeet = new HashSet<>();

  public ValintakoeValinnanvaihe() {}

  @PersistenceCreator
  public ValintakoeValinnanvaihe(Set<Valintakoe> valintakokeet) {
    this.valintakokeet.addAll(valintakokeet);
  }

  public String getValinnanvaiheOid() {
    return valinnanvaiheOid;
  }

  public void setValinnanvaiheOid(String valinnanvaiheOid) {
    this.valinnanvaiheOid = valinnanvaiheOid;
  }

  public Integer getValinnanVaiheJarjestysluku() {
    return valinnanVaiheJarjestysluku;
  }

  public void setValinnanVaiheJarjestysluku(Integer valinnanVaiheJarjestysluku) {
    this.valinnanVaiheJarjestysluku = valinnanVaiheJarjestysluku;
  }

  public Set<Valintakoe> getValintakokeet() {
    return valintakokeet;
  }

  public List<Valintakoe> getValintakokeetAsList() {
    return new ArrayList<>(valintakokeet);
  }

  public void setValintakokeet(List<Valintakoe> valintakokeet) {
    this.valintakokeet.clear();
    this.valintakokeet.addAll(valintakokeet);
  }

  public Hakutoive getHakutoive() {
    return hakutoive;
  }

  public void setHakutoive(Hakutoive hakutoive) {
    this.hakutoive = hakutoive;
  }

  @Override
  public String toString() {
    return ToStringBuilder.reflectionToString(this);
  }
}
