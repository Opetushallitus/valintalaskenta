package fi.vm.sade.valintalaskenta.domain.valinta;

import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.Tasasijasaanto;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import javax.persistence.*;

@Entity(name = "Valintatapajono")
public class Valintatapajono {

  @Id
  @GeneratedValue(strategy = GenerationType.AUTO)
  private UUID id;

  @Column
  private String valintatapajonoOid;

  @Column
  private String nimi;

  @Column
  private int prioriteetti;

  @Column
  private int aloituspaikat;

  /**
   * Valintaperusteissa ylläpidettävä tieto siitä, onko tätä jonoa ylipäätään tarkoitus sijoitella.
   */
  @Column
  private boolean siirretaanSijoitteluun;

  @Column
  private Tasasijasaanto tasasijasaanto;

  @Column
  private Boolean eiVarasijatayttoa;

  @Column
  private Boolean kaikkiEhdonTayttavatHyvaksytaan;

  @Column
  private Boolean kaytetaanValintalaskentaa;

  @Column
  private Boolean poissaOlevaTaytto;

  /**
   * Valintalaskennan tuloksissa ylläpidettävä tieto siitä, otetaanko jono mukaan seuraavaan
   * sijoitteluajoon vai ei.
   */
  @Column
  private Boolean valmisSijoiteltavaksi = true;

  @Column
  private Boolean kaytetaanKokonaispisteita;

  @OneToMany(mappedBy = "valintatapajono")
  private List<Jonosija> jonosijat;

  @ManyToOne
  private Valinnanvaihe valinnanvaihe;

  @Column
  private Long sijoitteluajoId;

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

  public List<Jonosija> getJonosijat() {
    if (null == jonosijat) {
      //TODO Check necessity of this exception
      throw new IllegalStateException(
          String.format(
              "Jonosijat not loaded for valintatapajono %s with jonosijaids %s",
              valintatapajonoOid, jonosijat));
    }
    return jonosijat;
  }

  public void setJonosijat(List<Jonosija> jonosijat) {
    this.jonosijat = jonosijat;
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
}
