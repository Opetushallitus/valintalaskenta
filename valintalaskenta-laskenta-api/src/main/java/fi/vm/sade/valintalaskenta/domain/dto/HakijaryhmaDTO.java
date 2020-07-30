package fi.vm.sade.valintalaskenta.domain.dto;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

public class HakijaryhmaDTO {
  private String nimi;

  private String hakijaryhmaOid;

  private int prioriteetti;

  private Date createdAt;

  private String hakukohdeOid;

  private String kuvaus;

  private int kiintio;

  private boolean kaytaKaikki;

  private boolean tarkkaKiintio;

  private boolean kaytetaanRyhmaanKuuluvia;

  private String hakijaryhmatyyppikoodiUri;

  private String valintatapajonoOid;

  private List<JonosijaDTO> jonosijat = new ArrayList<JonosijaDTO>();

  public String getNimi() {
    return nimi;
  }

  public void setNimi(String nimi) {
    this.nimi = nimi;
  }

  public Integer getPrioriteetti() {
    return prioriteetti;
  }

  public void setPrioriteetti(Integer prioriteetti) {
    this.prioriteetti = prioriteetti;
  }

  public String getHakijaryhmaOid() {
    return hakijaryhmaOid;
  }

  public void setHakijaryhmaOid(String hakijaryhmaOid) {
    this.hakijaryhmaOid = hakijaryhmaOid;
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

  public List<JonosijaDTO> getJonosijat() {
    return jonosijat;
  }

  public void setJonosijat(List<JonosijaDTO> jonosijat) {
    this.jonosijat = jonosijat;
  }

  public String getHakijaryhmatyyppikoodiUri() {
    return hakijaryhmatyyppikoodiUri;
  }

  public void setHakijaryhmatyyppikoodiUri(String hakijaryhmatyyppikoodiUri) {
    this.hakijaryhmatyyppikoodiUri = hakijaryhmatyyppikoodiUri;
  }
}
