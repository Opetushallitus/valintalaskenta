package fi.vm.sade.valintalaskenta.domain.valinta;

import javax.persistence.*;
import java.util.*;
import java.util.stream.Collectors;

@Entity(name="Hakijaryhma")
public class Hakijaryhma {

  @Id
  @GeneratedValue(strategy = GenerationType.AUTO)
  private UUID id;

  @Column
  private String hakijaryhmaOid;

  @Column
  private int prioriteetti;
  @Column
  private Date createdAt;

  //@Indexed(unique = false, dropDups = false)

  @Column
  private String hakukohdeOid;

  @Column
  private String nimi;

  @Column
  private String kuvaus;

  @Column
  private int kiintio;

  @Column
  private boolean kaytaKaikki;

  @Column
  private boolean tarkkaKiintio;

  @Column
  private boolean kaytetaanRyhmaanKuuluvia;

  @Column
  private String hakijaryhmatyyppiKoodiuri;

  @Column
  private String valintatapajonoOid;

  @OneToMany(mappedBy = "hakijaryhma")
  private List<Jonosija> jonosijat = new ArrayList<>();

  public Date getCreatedAt() {
    return createdAt;
  }

  public void setCreatedAt(Date createdAt) {
    this.createdAt = createdAt;
  }

  public String getHakukohdeOid() {
    return hakukohdeOid;
  }

  public void setHakukohdeOid(String hakukohdeOid) {
    this.hakukohdeOid = hakukohdeOid;
  }

  public String getNimi() {
    return nimi;
  }

  public void setNimi(String nimi) {
    this.nimi = nimi;
  }

  public String getHakijaryhmaOid() {
    return hakijaryhmaOid;
  }

  public void setHakijaryhmaOid(String hakijaryhmaOid) {
    this.hakijaryhmaOid = hakijaryhmaOid;
  }

  public int getPrioriteetti() {
    return prioriteetti;
  }

  public void setPrioriteetti(int prioriteetti) {
    this.prioriteetti = prioriteetti;
  }

  public String getKuvaus() {
    return kuvaus;
  }

  public void setKuvaus(String kuvaus) {
    this.kuvaus = kuvaus;
  }

  public int getKiintio() {
    return kiintio;
  }

  public void setKiintio(int kiintio) {
    this.kiintio = kiintio;
  }

  public boolean isKaytaKaikki() {
    return kaytaKaikki;
  }

  public void setKaytaKaikki(boolean kaytaKaikki) {
    this.kaytaKaikki = kaytaKaikki;
  }

  public boolean isTarkkaKiintio() {
    return tarkkaKiintio;
  }

  public void setTarkkaKiintio(boolean tarkkaKiintio) {
    this.tarkkaKiintio = tarkkaKiintio;
  }

  public boolean isKaytetaanRyhmaanKuuluvia() {
    return kaytetaanRyhmaanKuuluvia;
  }

  public void setKaytetaanRyhmaanKuuluvia(boolean kaytetaanRyhmaanKuuluvia) {
    this.kaytetaanRyhmaanKuuluvia = kaytetaanRyhmaanKuuluvia;
  }

  public String getValintatapajonoOid() {
    return valintatapajonoOid;
  }

  public void setValintatapajonoOid(String valintatapajonoOid) {
    this.valintatapajonoOid = valintatapajonoOid;
  }

  public List<String> getJonosijaIdt() {
    return jonosijat == null ? new ArrayList<>() : jonosijat.stream().map(Jonosija::getId).collect(Collectors.toList());
  }

  public List<Jonosija> getJonosijat() {
    if (null == jonosijat) {
      throw new IllegalStateException(
          String.format(
              "Jonosijat not loaded for hakijaryhma %s with jonosijaids %s",
              hakijaryhmaOid, jonosijat));
    }
    return jonosijat;
  }

  public void setJonosijat(List<Jonosija> jonosijat) {
    this.jonosijat = jonosijat;
  }

  public String getHakijaryhmatyyppiKoodiuri() {
    return hakijaryhmatyyppiKoodiuri;
  }

  public void setHakijaryhmatyyppiKoodiuri(String hakijaryhmatyyppikoodiUri) {
    this.hakijaryhmatyyppiKoodiuri = hakijaryhmatyyppikoodiUri;
  }

  public void setId(UUID id) {
    this.id = id;
  }

  public UUID getId() {
    return id;
  }
}
