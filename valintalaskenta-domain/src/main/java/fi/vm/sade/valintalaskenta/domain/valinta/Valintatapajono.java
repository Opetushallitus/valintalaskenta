package fi.vm.sade.valintalaskenta.domain.valinta;

import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.Tasasijasaanto;
import java.util.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.PersistenceCreator;
import org.springframework.data.annotation.Transient;

public class Valintatapajono {

  @Id private UUID id;

  private String valintatapajonoOid;

  private String nimi;

  private int prioriteetti;

  private int aloituspaikat;

  private boolean siirretaanSijoitteluun;

  private Tasasijasaanto tasasijasaanto;

  private Boolean eiVarasijatayttoa;

  private Boolean kaikkiEhdonTayttavatHyvaksytaan;

  private Boolean kaytetaanValintalaskentaa;

  private Boolean poissaOlevaTaytto;

  /**
   * Valintalaskennan tuloksissa ylläpidettävä tieto siitä, otetaanko jono mukaan seuraavaan
   * sijoitteluajoon vai ei.
   */
  private Boolean valmisSijoiteltavaksi = true;

  private Boolean kaytetaanKokonaispisteita;

  private Set<Jonosija> jonosija = new HashSet<>();

  @Transient private Valinnanvaihe valinnanvaihe;

  private Long sijoitteluajoId;

  private Date lastModified;

  public Valintatapajono() {}

  @PersistenceCreator
  public Valintatapajono(Set<Jonosija> jonosija) {
    this.jonosija.addAll(jonosija);
  }

  public void setId(UUID id) {
    this.id = id;
  }

  public UUID getId() {
    return id;
  }

  public String getValintatapajonoOid() {
    return valintatapajonoOid;
  }

  public void setValintatapajonoOid(String valintatapajonoOid) {
    this.valintatapajonoOid = valintatapajonoOid;
  }

  public String getNimi() {
    return nimi;
  }

  public void setNimi(String nimi) {
    this.nimi = nimi;
  }

  public int getPrioriteetti() {
    return prioriteetti;
  }

  public void setPrioriteetti(int prioriteetti) {
    this.prioriteetti = prioriteetti;
  }

  public int getAloituspaikat() {
    return aloituspaikat;
  }

  public void setAloituspaikat(int aloituspaikat) {
    this.aloituspaikat = aloituspaikat;
  }

  public boolean isSiirretaanSijoitteluun() {
    return siirretaanSijoitteluun;
  }

  public void setSiirretaanSijoitteluun(boolean siirretaanSijoitteluun) {
    this.siirretaanSijoitteluun = siirretaanSijoitteluun;
  }

  public Tasasijasaanto getTasasijasaanto() {
    return tasasijasaanto;
  }

  public void setTasasijasaanto(Tasasijasaanto tasasijasaanto) {
    this.tasasijasaanto = tasasijasaanto;
  }

  public Boolean getEiVarasijatayttoa() {
    return eiVarasijatayttoa;
  }

  public void setEiVarasijatayttoa(Boolean eiVarasijatayttoa) {
    this.eiVarasijatayttoa = eiVarasijatayttoa;
  }

  public Set<Jonosija> getJonosijat() {
    return jonosija;
  }

  public List<Jonosija> getJonosijatAsList() {
    return new ArrayList<>(jonosija);
  }

  public void setJonosijat(Set<Jonosija> jonosijat) {
    if (jonosijat == null) {
      this.jonosija = null;
    } else {
      this.jonosija.clear();
      this.jonosija.addAll(jonosijat);
    }
  }

  public Boolean getKaikkiEhdonTayttavatHyvaksytaan() {
    return kaikkiEhdonTayttavatHyvaksytaan;
  }

  public void setKaikkiEhdonTayttavatHyvaksytaan(Boolean kaikkiEhdonTayttavatHyvaksytaan) {
    this.kaikkiEhdonTayttavatHyvaksytaan = kaikkiEhdonTayttavatHyvaksytaan;
  }

  public Boolean getPoissaOlevaTaytto() {
    return poissaOlevaTaytto;
  }

  public void setPoissaOlevaTaytto(Boolean poissaOlevaTaytto) {
    this.poissaOlevaTaytto = poissaOlevaTaytto;
  }

  public Boolean getKaytetaanValintalaskentaa() {
    return kaytetaanValintalaskentaa;
  }

  public void setKaytetaanValintalaskentaa(Boolean kaytetaanValintalaskentaa) {
    this.kaytetaanValintalaskentaa = kaytetaanValintalaskentaa;
  }

  public Boolean getKaytetaanKokonaispisteita() {
    return kaytetaanKokonaispisteita;
  }

  public void setKaytetaanKokonaispisteita(Boolean kaytetaanKokonaispisteita) {
    this.kaytetaanKokonaispisteita = kaytetaanKokonaispisteita;
  }

  public Boolean getValmisSijoiteltavaksi() {
    return valmisSijoiteltavaksi;
  }

  public void setValmisSijoiteltavaksi(Boolean valmisSijoiteltavaksi) {
    this.valmisSijoiteltavaksi = valmisSijoiteltavaksi;
  }

  public Long getSijoitteluajoId() {
    return sijoitteluajoId;
  }

  public void setSijoitteluajoId(Long sijoitteluajoId) {
    this.sijoitteluajoId = sijoitteluajoId;
  }

  public Valinnanvaihe getValinnanVaihe() {
    return valinnanvaihe;
  }

  public void setValinnanVaihe(Valinnanvaihe valinnanVaihe) {
    this.valinnanvaihe = valinnanVaihe;
  }

  public Date getLastModified() {
    return lastModified;
  }

  public void setLastModified(Date lastModified) {
    this.lastModified = lastModified;
  }
}
