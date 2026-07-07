package fi.vm.sade.valintalaskenta.domain.valinta;

import java.util.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.Transient;
import org.springframework.data.domain.Persistable;
import org.springframework.data.relational.core.mapping.Column;

// Implements Persistable jotta Spring Data JDBC osaa erottaa uuden ja olemassaolevan
// entiteetin toisistaan, kun id on valmiiksi asetettu (ks. isNew/markNotNew alla).
public class Jonosija implements Persistable<UUID> {

  // UUID generoidaan Javassa eikä kannassa, jotta Spring Data JDBC voi tehdä
  // INSERT:n ilman RETURNING *. Ilman tätä kanta generoi UUID:n ja ajuri joutuu
  // lukemaan koko rivin (ml. isot JSONB-sarakkeet) takaisin jokaisen insertin jälkeen,
  // mikä aiheuttaa Client:ClientWrite -odotuksen Aurorassa.
  @Id private UUID id = UUID.randomUUID();

  // Spring Data JDBC päättelee normaalisti INSERT vs UPDATE sen perusteella onko id null.
  // Koska id on aina asetettu (ks. yllä), tarvitaan erillinen lippu kertomaan milloin
  // kyseessä on uusi entiteetti. markNotNew() kutsutaan AfterConvertCallback:ssa kun
  // entiteetti ladataan kannasta (DatabaseConfiguration).
  @Transient private boolean isNew = true;

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

  // @ReadOnlyProperty on tarkodeliberaattisesti poistettu: se pakottaisi Spring Data JDBC:n
  // käyttämään RETURNING * myös lastModified:n takia. Arvo asetetaan alla konstruktorissa,
  // mutta tietokanta ylikirjoittaa sen set_last_modified-triggerillä ennen insertiä.
  private Date lastModified;

  public Jonosija() {
    lastModified = new Date();
    syotetytArvot = new SyotettyArvoContainer();
    funktioTulokset = new FunktioTulosContainer();
    jarjestyskriteeritulokset = new JarjestyskriteeritulosContainer();
  }

  @Override
  public UUID getId() {
    return this.id;
  }

  @Override
  public boolean isNew() {
    return isNew;
  }

  public void markNotNew() {
    isNew = false;
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
