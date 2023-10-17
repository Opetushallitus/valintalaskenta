package fi.vm.sade.valintalaskenta.domain.valintakoe;

import fi.vm.sade.service.valintaperusteet.dto.model.Koekutsu;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.Transient;

public class Valintakoe {

  @Id private UUID id;

  private String valintakoeOid;

  private String valintakoeTunniste;

  private String nimi;
  private boolean aktiivinen;

  private Osallistuminen osallistuminen;

  private boolean lahetetaankoKoekutsut;

  private Integer kutsuttavienMaara;

  private Koekutsu kutsunKohde = Koekutsu.YLIN_TOIVE;

  private String kutsunKohdeAvain;

  private String kuvausFI;
  private String kuvausEN;

  private String kuvausSV;

  private String laskentaTila;

  private Boolean laskentaTulos;

  private String tekninenKuvaus;

  @Transient private ValintakoeValinnanvaihe valintakoeValinnanvaihe;

  public Valintakoe() {}

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
    Map<String, String> kuvaus = new HashMap<>();
    if (getKuvausFI() != null) {
      kuvaus.put("FI", getKuvausFI());
    }
    if (getKuvausSV() != null) {
      kuvaus.put("SV", getKuvausSV());
    }
    if (getKuvausEN() != null) {
      kuvaus.put("EN", getKuvausEN());
    }
    return kuvaus;
  }

  public void setKuvaus(Map<String, String> kuvaus) {
    if (kuvaus != null) {
      if (kuvaus.get("FI") != null) {
        this.setKuvausFI(kuvaus.get("FI"));
      }
      if (kuvaus.get("SV") != null) {
        this.setKuvausEN(kuvaus.get("EN"));
      }
      if (kuvaus.get("EN") != null) {
        this.setKuvausSV(kuvaus.get("SV"));
      }
    }
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
