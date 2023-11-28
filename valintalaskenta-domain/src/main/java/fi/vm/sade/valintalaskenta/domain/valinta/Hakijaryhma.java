package fi.vm.sade.valintalaskenta.domain.valinta;

import java.util.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.PersistenceCreator;

public class Hakijaryhma {

  @Id public UUID id;

  public String hakijaryhmaOid;

  public int prioriteetti;

  public Date createdAt = new Date();

  public String hakukohdeOid;

  public String nimi;

  public String kuvaus;

  public int kiintio;

  public boolean kaytaKaikki;

  public boolean tarkkaKiintio;
  public boolean kaytetaanRyhmaanKuuluvia;

  public String hakijaryhmatyyppiKoodiuri;

  public String valintatapajonoOid;

  public final Set<Jonosija> jonosija = new HashSet<>();

  @PersistenceCreator
  public Hakijaryhma(Collection<Jonosija> jonosija) {
    this.jonosija.addAll(jonosija);
  }

  public Hakijaryhma() {}

  public void setJonosija(Collection<Jonosija> jonosija) {
    this.jonosija.clear();
    this.jonosija.addAll(jonosija);
  }

  public UUID getId() {
    return id;
  }

  public void setId(UUID id) {
    this.id = id;
  }

  public Date getCreatedAt() {
    return createdAt;
  }

  public int getKiintio() {
    return kiintio;
  }

  public void setKiintio(int kiintio) {
    this.kiintio = kiintio;
  }

  public int getPrioriteetti() {
    return prioriteetti;
  }

  public void setPrioriteetti(int prioriteetti) {
    this.prioriteetti = prioriteetti;
  }

  public Set<Jonosija> getJonosijat() {
    return jonosija;
  }

  public String getHakijaryhmaOid() {
    return hakijaryhmaOid;
  }

  public void setHakijaryhmaOid(String hakijaryhmaOid) {
    this.hakijaryhmaOid = hakijaryhmaOid;
  }

  public String getHakijaryhmatyyppiKoodiuri() {
    return hakijaryhmatyyppiKoodiuri;
  }

  public void setHakijaryhmatyyppiKoodiuri(String hakijaryhmatyyppiKoodiuri) {
    this.hakijaryhmatyyppiKoodiuri = hakijaryhmatyyppiKoodiuri;
  }

  public String getHakukohdeOid() {
    return hakukohdeOid;
  }

  public void setHakukohdeOid(String hakukohdeOid) {
    this.hakukohdeOid = hakukohdeOid;
  }

  public String getKuvaus() {
    return kuvaus;
  }

  public void setKuvaus(String kuvaus) {
    this.kuvaus = kuvaus;
  }

  public String getNimi() {
    return nimi;
  }

  public void setNimi(String nimi) {
    this.nimi = nimi;
  }

  public String getValintatapajonoOid() {
    return valintatapajonoOid;
  }

  public void setValintatapajonoOid(String valintatapajonoOid) {
    this.valintatapajonoOid = valintatapajonoOid;
  }

  public boolean isKaytaKaikki() {
    return kaytaKaikki;
  }

  public boolean isKaytetaanRyhmaanKuuluvia() {
    return kaytetaanRyhmaanKuuluvia;
  }

  public boolean isTarkkaKiintio() {
    return tarkkaKiintio;
  }

  public void setCreatedAt(Date createdAt) {
    this.createdAt = createdAt;
  }

  public void setKaytaKaikki(boolean kaytaKaikki) {
    this.kaytaKaikki = kaytaKaikki;
  }

  public void setKaytetaanRyhmaanKuuluvia(boolean kaytetaanRyhmaanKuuluvia) {
    this.kaytetaanRyhmaanKuuluvia = kaytetaanRyhmaanKuuluvia;
  }

  public void setTarkkaKiintio(boolean tarkkaKiintio) {
    this.tarkkaKiintio = tarkkaKiintio;
  }
}
