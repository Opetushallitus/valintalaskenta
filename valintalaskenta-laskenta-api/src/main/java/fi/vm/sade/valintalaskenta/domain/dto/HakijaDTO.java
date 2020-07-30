package fi.vm.sade.valintalaskenta.domain.dto;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

public class HakijaDTO {
  private int jonosija;
  private String oid;
  private String hakemusOid;
  private JarjestyskriteerituloksenTilaDTO tila;
  private int prioriteetti;
  private String etunimi;
  private String sukunimi;
  private Integer tasasijaJonosija;
  private BigDecimal pisteet;
  private List<SyotettyArvoDTO> syotettyArvo;
  private List<AvainArvoDTO> tilanKuvaus;
  private Boolean harkinnanvarainen = false;
  private boolean hylattyValisijoittelussa = false;

  public HakijaDTO() {}

  public HakijaDTO(
      final int jonosija,
      final String oid,
      final String hakemusOid,
      final JarjestyskriteerituloksenTilaDTO tila,
      final int prioriteetti,
      final String etunimi,
      final String sukunimi,
      final Integer tasasijaJonosija,
      final BigDecimal pisteet,
      final List<SyotettyArvoDTO> syotettyArvo,
      final List<AvainArvoDTO> tilanKuvaus,
      final Boolean harkinnanvarainen,
      final boolean hylattyValisijoittelussa) {
    this.jonosija = jonosija;
    this.oid = oid;
    this.hakemusOid = hakemusOid;
    this.tila = tila;
    this.prioriteetti = prioriteetti;
    this.etunimi = etunimi;
    this.sukunimi = sukunimi;
    this.tasasijaJonosija = tasasijaJonosija;
    this.pisteet = pisteet;
    this.syotettyArvo = syotettyArvo;
    this.tilanKuvaus = tilanKuvaus;
    this.harkinnanvarainen = harkinnanvarainen;
    this.hylattyValisijoittelussa = hylattyValisijoittelussa;
  }

  public int getJonosija() {
    return jonosija;
  }

  public void setJonosija(int jonosija) {
    this.jonosija = jonosija;
  }

  public String getOid() {
    return oid;
  }

  public void setOid(String oid) {
    this.oid = oid;
  }

  public String getHakemusOid() {
    return hakemusOid;
  }

  public void setHakemusOid(String hakemusOid) {
    this.hakemusOid = hakemusOid;
  }

  public JarjestyskriteerituloksenTilaDTO getTila() {
    return tila;
  }

  public void setTila(JarjestyskriteerituloksenTilaDTO tila) {
    this.tila = tila;
  }

  public int getPrioriteetti() {
    return prioriteetti;
  }

  public void setPrioriteetti(int prioriteetti) {
    this.prioriteetti = prioriteetti;
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

  public Integer getTasasijaJonosija() {
    return tasasijaJonosija;
  }

  public void setTasasijaJonosija(Integer tasasijaJonosija) {
    this.tasasijaJonosija = tasasijaJonosija;
  }

  public BigDecimal getPisteet() {
    return pisteet;
  }

  public void setPisteet(BigDecimal pisteet) {
    this.pisteet = pisteet;
  }

  public List<SyotettyArvoDTO> getSyotettyArvo() {
    if (syotettyArvo == null) {
      syotettyArvo = new ArrayList<SyotettyArvoDTO>();
    }
    return syotettyArvo;
  }

  public void setSyotettyArvo(List<SyotettyArvoDTO> syotettyArvo) {
    this.syotettyArvo = syotettyArvo;
  }

  public List<AvainArvoDTO> getTilanKuvaus() {
    if (tilanKuvaus == null) {
      tilanKuvaus = new ArrayList<AvainArvoDTO>();
    }
    return tilanKuvaus;
  }

  public void setTilanKuvaus(List<AvainArvoDTO> tilanKuvaus) {
    this.tilanKuvaus = tilanKuvaus;
  }

  public void setHarkinnanvarainen(Boolean harkinnanvarainen) {
    this.harkinnanvarainen = harkinnanvarainen;
  }

  public Boolean getHarkinnanvarainen() {
    return harkinnanvarainen;
  }

  public boolean isHylattyValisijoittelussa() {
    return hylattyValisijoittelussa;
  }

  public void setHylattyValisijoittelussa(boolean hylattyValisijoittelussa) {
    this.hylattyValisijoittelussa = hylattyValisijoittelussa;
  }
}
