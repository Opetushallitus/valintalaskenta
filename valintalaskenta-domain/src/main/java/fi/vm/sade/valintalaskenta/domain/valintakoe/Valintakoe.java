package fi.vm.sade.valintalaskenta.domain.valintakoe;

import fi.vm.sade.service.valintaperusteet.dto.model.Koekutsu;
import org.apache.commons.lang3.builder.ToStringBuilder;

import javax.persistence.*;
import java.util.Map;
import java.util.UUID;

@Entity(name = "Valintakoe")
public class Valintakoe {

  @Id
  @GeneratedValue(strategy = GenerationType.AUTO)
  private UUID id;

  @Column
  private String valintakoeOid;

  @Column
  private String valintakoeTunniste;

  @Column
  private String nimi;
  private boolean aktiivinen;

  @Column
  private Osallistuminen osallistuminen;

  @Column
  private boolean lahetetaankoKoekutsut;

  @Column
  private Integer kutsuttavienMaara;

  @Column
  private Koekutsu kutsunKohde = Koekutsu.YLIN_TOIVE;

  @Column
  private String kutsunKohdeAvain;

  @Column
  private String kuvausFI;

  @Column
  private String kuvausEN;

  @Column
  private String kuvausSV;

  @Column
  private String laskentaTila;

  @Column
  private Boolean laskentaTulos;

  @Column
  private String tekninenKuvaus;

  @ManyToOne
  private ValintakoeValinnanvaihe valintakoeValinnanvaihe;

  public boolean isAktiivinen() {
    return aktiivinen;
  }

  public void setAktiivinen(boolean aktiivinen) {
    this.aktiivinen = aktiivinen;
  }

  public String getNimi() {
    return nimi;
  }

  public void setNimi(String nimi) {
    this.nimi = nimi;
  }

  public String getValintakoeOid() {
    return valintakoeOid;
  }

  public void setValintakoeOid(String valintakoeOid) {
    this.valintakoeOid = valintakoeOid;
  }

  public String getValintakoeTunniste() {
    return valintakoeTunniste;
  }

  public void setValintakoeTunniste(String valintakoeTunniste) {
    this.valintakoeTunniste = valintakoeTunniste;
  }

  public Osallistuminen getOsallistuminen() {
    return osallistuminen;
  }

  public void setOsallistuminen(Osallistuminen osallistuminen) {
    this.osallistuminen = osallistuminen;
  }

  public boolean isLahetetaankoKoekutsut() {
    return lahetetaankoKoekutsut;
  }

  public void setLahetetaankoKoekutsut(boolean lahetetaankoKoekutsut) {
    this.lahetetaankoKoekutsut = lahetetaankoKoekutsut;
  }

  public Integer getKutsuttavienMaara() {
    return kutsuttavienMaara;
  }

  public void setKutsuttavienMaara(final Integer kutsuttavienMaara) {
    this.kutsuttavienMaara = kutsuttavienMaara;
  }

  public Koekutsu getKutsunKohde() {
    return kutsunKohde;
  }

  public void setKutsunKohde(Koekutsu kutsunKohde) {
    this.kutsunKohde = kutsunKohde;
  }

  public String getKutsunKohdeAvain() {
    return kutsunKohdeAvain;
  }

  public void setKutsunKohdeAvain(String kutsunKohdeAvain) {
    this.kutsunKohdeAvain = kutsunKohdeAvain;
  }

  public UUID getId() {
    return id;
  }

  public void setId(UUID id) {
    this.id = id;
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

  public String getKuvausEN() {
    return kuvausEN;
  }

  public void setKuvausEN(String kuvausEN) {
    this.kuvausEN = kuvausEN;
  }

  public Boolean getLaskentaTulos() {
    return laskentaTulos;
  }

  public void setLaskentaTulos(Boolean laskentaTulos) {
    this.laskentaTulos = laskentaTulos;
  }

  public String getLaskentaTila() {
    return laskentaTila;
  }

  public void setLaskentaTila(String laskentatila) {
    this.laskentaTila = laskentatila;
  }

  public String getTekninenKuvaus() {
    return tekninenKuvaus;
  }

  public void setTekninenKuvaus(String tekninenKuvaus) {
    this.tekninenKuvaus = tekninenKuvaus;
  }

  public Map<String, String> getKuvaus() {
    return Map.of("FI", getKuvausFI(), "SV", getKuvausSV(), "EN", getKuvausEN());
  }

  public void setKuvaus(Map<String, String> kuvaus) {
    this.setKuvausFI(kuvaus.get("FI"));
    this.setKuvausEN(kuvaus.get("EN"));
    this.setKuvausSV(kuvaus.get("SV"));
  }

  public OsallistuminenTulos getOsallistuminenTulos() {
    OsallistuminenTulos tulos = new OsallistuminenTulos();
    tulos.setLaskentaTulos(getLaskentaTulos());
    tulos.setOsallistuminen(getOsallistuminen());
    tulos.setKuvaus(getKuvaus());
    tulos.setLaskentaTila(getLaskentaTila());
    tulos.setTekninenKuvaus(getTekninenKuvaus());
    return tulos;
  }

  @Override
  public String toString() {
    return ToStringBuilder.reflectionToString(this);
  }

  public void setOsallistuminenTulos(OsallistuminenTulos osallistuminenTulos) {
    setLaskentaTulos(osallistuminenTulos.getLaskentaTulos());
    setOsallistuminen(osallistuminenTulos.getOsallistuminen());
    setKuvaus(osallistuminenTulos.getKuvaus());
    setLaskentaTila(osallistuminenTulos.getLaskentaTila());
    setTekninenKuvaus(osallistuminenTulos.getTekninenKuvaus());
  }
}
