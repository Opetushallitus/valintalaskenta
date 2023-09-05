package fi.vm.sade.valintalaskenta.domain.valinta;

import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.Tasasijasaanto;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.*;

public class Valintatapajono {
  public static final int CURRENT_SCHEMA_VERSION = 2;

  private String id;

  private int schemaVersion = CURRENT_SCHEMA_VERSION;

  //@Indexed
  private String valintatapajonoOid;

  private String nimi;

  private int prioriteetti;

  private int aloituspaikat;

  /**
   * Valintaperusteissa ylläpidettävä tieto siitä, onko tätä jonoa ylipäätään tarkoitus sijoitella.
   */
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

  private List<String> jonosijaIdt;

  @Transient private List<Jonosija> jonosijat;

  private Long sijoitteluajoId;

  public void setId(String id) {
    this.id = id;
  }

  public String getId() {
    return id;
  }

  public int getSchemaVersion() {
    return schemaVersion;
  }

  public void setSchemaVersion(int schemaVersion) {
    this.schemaVersion = schemaVersion;
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

  public List<String> getJonosijaIdt() {
    return jonosijaIdt == null ? new ArrayList<>() : jonosijaIdt;
  }

  public void setJonosijaIdt(List<String> jonosijaIdt) {
    this.jonosijaIdt = jonosijaIdt;
  }

  public List<Jonosija> getJonosijat() {
    if (null == jonosijat) {
      throw new IllegalStateException(
          String.format(
              "Jonosijat not loaded for valintatapajono %s with jonosijaids %s",
              valintatapajonoOid, jonosijaIdt));
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
}
