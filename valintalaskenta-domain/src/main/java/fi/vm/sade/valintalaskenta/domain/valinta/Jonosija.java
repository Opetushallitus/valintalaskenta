package fi.vm.sade.valintalaskenta.domain.valinta;

import java.util.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.ReadOnlyProperty;
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

  @Column("jarjestyskriteeritulokset")
  private JarjestyskriteeritulosContainer jarjestyskriteeritulokset;

  @Column("syotetyt_arvot")
  private SyotettyArvoContainer syotetytArvot;

  @Column("funktio_tulokset")
  private FunktioTulosContainer funktioTulokset;

  @ReadOnlyProperty private Date lastModified;

  public Jonosija() {
    syotetytArvot = new SyotettyArvoContainer();
    funktioTulokset = new FunktioTulosContainer();
    jarjestyskriteeritulokset = new JarjestyskriteeritulosContainer();
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

  public JarjestyskriteeritulosContainer getJarjestyskriteeritulokset() {
    if (jarjestyskriteeritulokset == null) {
      jarjestyskriteeritulokset = new JarjestyskriteeritulosContainer();
    }
    return jarjestyskriteeritulokset;
  }

  public void setJarjestyskriteeritulokset(
      JarjestyskriteeritulosContainer jarjestyskriteeritulokset) {
    this.jarjestyskriteeritulokset = jarjestyskriteeritulokset;
    if (jarjestyskriteeritulokset != null) {
      jarjestaJarjestyskriteeritulokset();
    }
  }

  private void jarjestaJarjestyskriteeritulokset() {
    Collections.sort(
        jarjestyskriteeritulokset.jarjestyskriteeritulokset,
        Comparator.comparingInt(Jarjestyskriteeritulos::getPrioriteetti));
  }

  public List<SyotettyArvo> getSyotetytArvot() {
    if (syotetytArvot == null) {
      syotetytArvot = new SyotettyArvoContainer();
    }
    return syotetytArvot.syotetytArvot;
  }

  public void setSyotetytArvot(SyotettyArvoContainer syotetytArvot) {
    this.syotetytArvot = syotetytArvot;
  }

  public FunktioTulosContainer getFunktioTulokset() {
    if (funktioTulokset == null) {
      funktioTulokset = new FunktioTulosContainer();
    }
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

  public Date getLastModified() {
    return lastModified;
  }

  public void setLastModified(Date lastModified) {
    this.lastModified = lastModified;
  }
}
