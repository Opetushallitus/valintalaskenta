package fi.vm.sade.valintalaskenta.domain.valinta;

import java.util.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.PersistenceCreator;
import org.springframework.data.annotation.Transient;
import org.springframework.data.relational.core.mapping.Column;

public class Jonosija {

  @Id private UUID id;

  private String hakemusOid;

  private String hakijaOid;

  private int hakutoiveprioriteetti; // hakutoive

  private boolean harkinnanvarainen = false;

  private boolean hylattyValisijoittelussa = false;

  @Transient private Hakijaryhma hakijaryhma;

  @Transient private Valintatapajono valintatapajono;

  private final List<Jarjestyskriteeritulos> jarjestyskriteeritulokset =
      new ArrayList<Jarjestyskriteeritulos>();

  @Column("syotetyt_arvot")
  private SyotettyArvoContainer syotetytArvot;

  @Column("funktio_tulokset")
  private FunktioTulosContainer funktioTulokset;

  public Jonosija() {
    syotetytArvot = new SyotettyArvoContainer();
    funktioTulokset = new FunktioTulosContainer();
  }

  @PersistenceCreator
  public Jonosija(List<Jarjestyskriteeritulos> jarjestyskriteeritulokset) {
    this.jarjestyskriteeritulokset.addAll(jarjestyskriteeritulokset);
    jarjestaJarjestyskriteeritulokset();
  }

  public UUID getId() {
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

  public List<Jarjestyskriteeritulos> getJarjestyskriteeritulokset() {
    return jarjestyskriteeritulokset;
  }

  public void setJarjestyskriteeritulokset(List<Jarjestyskriteeritulos> jarjestyskriteeritulokset) {
    this.jarjestyskriteeritulokset.clear();
    this.jarjestyskriteeritulokset.addAll(jarjestyskriteeritulokset);
    jarjestaJarjestyskriteeritulokset();
  }

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
    return syotetytArvot.syotetytArvot;
  }

  public void setSyotetytArvot(SyotettyArvoContainer syotetytArvot) {
    this.syotetytArvot = syotetytArvot;
  }

  public FunktioTulosContainer getFunktioTulokset() {
    return funktioTulokset;
  }

  public void setFunktioTulokset(FunktioTulosContainer funktioTulokset) {
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
