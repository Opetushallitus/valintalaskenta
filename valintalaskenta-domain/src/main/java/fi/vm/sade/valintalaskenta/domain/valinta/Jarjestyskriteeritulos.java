package fi.vm.sade.valintalaskenta.domain.valinta;

import java.math.BigDecimal;
import java.util.Map;
import java.util.UUID;

import org.apache.commons.lang3.builder.ToStringBuilder;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.ManyToOne;

@Entity
public class Jarjestyskriteeritulos {

  @Id
  private UUID id;

  @Column
  private int prioriteetti;

  @Column
  private BigDecimal arvo;

  @Column
  private JarjestyskriteerituloksenTila tila;

  @Column
  private String kuvausFI;

  @Column
  private String kuvausSV;

  @Column
  private String kuvausEN;

  @Column
  private String nimi;

  @Column
  private String tekninenKuvaus;

  @ManyToOne
  private Jonosija jonosija;

  @ManyToOne
  MuokattuJonosija muokattuJonosija;

  public int getPrioriteetti() {
    return prioriteetti;
  }

  public void setPrioriteetti(int prioriteetti) {
    this.prioriteetti = prioriteetti;
  }

  public BigDecimal getArvo() {
    return arvo;
  }

  public void setArvo(BigDecimal arvo) {
    this.arvo = arvo;
  }

  public JarjestyskriteerituloksenTila getTila() {
    return tila;
  }

  public void setTila(JarjestyskriteerituloksenTila tila) {
    this.tila = tila;
  }

  public String getNimi() {
    return nimi;
  }

  public void setNimi(String nimi) {
    this.nimi = nimi;
  }

  public Map<String, String> getKuvaus() {
    return Map.of("FI", getKuvausFI(), "SV", getKuvausSV(), "EN", getKuvausEN());
  }

  public void setKuvaus(Map<String, String> kuvaus) {
    this.setKuvausFI(kuvaus.get("FI"));
    this.setKuvausEN(kuvaus.get("EN"));
    this.setKuvausSV(kuvaus.get("SV"));
  }

  public String getTekninenKuvaus() {
    return tekninenKuvaus;
  }

  public void setTekninenKuvaus(String tekninenKuvaus) {
    this.tekninenKuvaus = tekninenKuvaus;
  }

  @Override
  public String toString() {
    return ToStringBuilder.reflectionToString(this);
  }

  public Jonosija getJonosija() {
    return jonosija;
  }

  public void setJonosija(Jonosija jonosija) {
    this.jonosija = jonosija;
  }

  public MuokattuJonosija getMuokattuJonosija() {
    return muokattuJonosija;
  }

  public void setMuokattuJonosija(MuokattuJonosija muokattuJonosija) {
    this.muokattuJonosija = muokattuJonosija;
  }

  public UUID getId() {
    return id;
  }

  public void setId(UUID id) {
    this.id = id;
  }

  public String getKuvausEN() {
    return kuvausEN;
  }

  public void setKuvausEN(String kuvausEn) {
    this.kuvausEN = kuvausEn;
  }

  public String getKuvausFI() {
    return kuvausFI;
  }

  public void setKuvausFI(String kuvausFI) {
    this.kuvausFI = kuvausFI;
  }

  public String getKuvausSV() {
    return kuvausSV;
  }

  public void setKuvausSV(String kuvausSV) {
    this.kuvausSV = kuvausSV;
  }
}
