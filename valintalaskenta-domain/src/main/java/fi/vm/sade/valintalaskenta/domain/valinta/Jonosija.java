package fi.vm.sade.valintalaskenta.domain.valinta;

import com.vladmihalcea.hibernate.type.json.JsonBinaryType;
import org.hibernate.annotations.Type;
import org.hibernate.annotations.TypeDef;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import javax.persistence.*;

@Entity(name = "Jonosija")
@TypeDef(name = "jsonb", typeClass = JsonBinaryType.class)
public class Jonosija {

  @Id
  private String id;

  @Column
  private String hakemusOid;

  @Column
  private String hakijaOid;

  @Column
  private String etunimi;

  @Column
  private String sukunimi;

  @Column
  private int hakutoiveprioriteetti; // hakutoive

  @Column
  private boolean harkinnanvarainen = false;

  @Column
  private boolean hylattyValisijoittelussa = false;

  @ManyToOne
  private Hakijaryhma hakijaryhma;

  @ManyToOne
  private Valintatapajono valintatapajono;

  @OneToMany(mappedBy = "jonosija")
  private List<Jarjestyskriteeritulos> jarjestyskriteeritulokset =
      new ArrayList<Jarjestyskriteeritulos>();

  @Type(type = "jsonb")
  @Column(columnDefinition = "jsonb")
  private List<SyotettyArvo> syotetytArvot = new ArrayList<SyotettyArvo>();

  @Type(type = "jsonb")
  @Column(columnDefinition = "jsonb")
  private List<FunktioTulos> funktioTulokset = new ArrayList<FunktioTulos>();

  public String getId() {
    return this.id;
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

  public String getEtunimi() {
    return etunimi;
  }

  public void setEtunimi(String etunimi) {
    this.etunimi = etunimi;
  }

  public String getSukunimi() {
    return sukunimi;
  }

  public void setSukunimi(String sukunimi) {
    this.sukunimi = sukunimi;
  }

  public int getHakutoiveprioriteetti() {
    return hakutoiveprioriteetti;
  }

  public void setHakutoiveprioriteetti(int hakutoiveprioriteetti) {
    this.hakutoiveprioriteetti = hakutoiveprioriteetti;
  }

  public boolean isHarkinnanvarainen() {
    return harkinnanvarainen;
  }

  public void setHarkinnanvarainen(boolean harkinnanvarainen) {
    this.harkinnanvarainen = harkinnanvarainen;
  }

  @OneToMany(mappedBy = "")
  public List<Jarjestyskriteeritulos> getJarjestyskriteeritulokset() {
    return jarjestyskriteeritulokset;
  }

  public void setJarjestyskriteeritulokset(List<Jarjestyskriteeritulos> jarjestyskriteeritulokset) {
    this.jarjestyskriteeritulokset = jarjestyskriteeritulokset;
  }

  @PrePersist
  private void jarjestaJarjestyskriteeritulokset() {
    Collections.sort(
        jarjestyskriteeritulokset,
        new Comparator<Jarjestyskriteeritulos>() {
          @Override
          public int compare(Jarjestyskriteeritulos o1, Jarjestyskriteeritulos o2) {
            return o1.getPrioriteetti() - o2.getPrioriteetti();
          }
        });
  }

  public List<SyotettyArvo> getSyotetytArvot() {
    return syotetytArvot;
  }

  public void setSyotetytArvot(List<SyotettyArvo> syotetytArvot) {
    this.syotetytArvot = syotetytArvot;
  }

  public List<FunktioTulos> getFunktioTulokset() {
    return funktioTulokset;
  }

  public void setFunktioTulokset(List<FunktioTulos> funktioTulokset) {
    this.funktioTulokset = funktioTulokset;
  }

  public boolean isHylattyValisijoittelussa() {
    return hylattyValisijoittelussa;
  }

  public void setHylattyValisijoittelussa(boolean hylattyValisijoittelussa) {
    this.hylattyValisijoittelussa = hylattyValisijoittelussa;
  }

  public Hakijaryhma getHakijaryhma() {
    return hakijaryhma;
  }

  public void setHakijaryhma(Hakijaryhma hakijaryhma) {
    this.hakijaryhma = hakijaryhma;
  }

  public Valintatapajono getValintatapajono() {
    return valintatapajono;
  }

  public void setValintatapajono(Valintatapajono valintatapajono) {
    this.valintatapajono = valintatapajono;
  }
}
