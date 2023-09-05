package fi.vm.sade.valintalaskenta.domain.valinta;

import java.util.*;
import java.util.stream.Collectors;

public class Hakijaryhma {
  public static final int CURRENT_SCHEMA_VERSION = 2;

  private Long id;

  private int schemaVersion = CURRENT_SCHEMA_VERSION;

  //@Indexed
  private String hakijaryhmaOid;

  private int prioriteetti;

  private Date createdAt;

  //@Indexed(unique = false, dropDups = false)
  private String hakukohdeOid;

  private String nimi;

  private String kuvaus;

  private int kiintio;

  private boolean kaytaKaikki;

  private boolean tarkkaKiintio;

  private boolean kaytetaanRyhmaanKuuluvia;

  private String hakijaryhmatyyppikoodiUri;

  private String valintatapajonoOid;

  private List<Jonosija> jonosijat = new ArrayList<>();

  private void prePersist() {
    createdAt = new Date();
  }

  public int getSchemaVersion() {
    return schemaVersion;
  }

  public void setSchemaVersion(int schemaVersion) {
    this.schemaVersion = schemaVersion;
  }

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

  public String getHakijaryhmatyyppikoodiUri() {
    return hakijaryhmatyyppikoodiUri;
  }

  public void setHakijaryhmatyyppikoodiUri(String hakijaryhmatyyppikoodiUri) {
    this.hakijaryhmatyyppikoodiUri = hakijaryhmatyyppikoodiUri;
  }

  public void setId(Long id) {
    this.id = id;
  }

  public Long getId() {
    return id;
  }
}
