package fi.vm.sade.valintalaskenta.domain.valintakoe;

import fi.vm.sade.service.valintaperusteet.dto.model.Koekutsu;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.mongodb.morphia.annotations.Embedded;

@Embedded
public class Valintakoe {
  private String valintakoeOid;
  private String valintakoeTunniste;
  private String nimi;
  private boolean aktiivinen;
  private OsallistuminenTulos osallistuminenTulos;
  private boolean lahetetaankoKoekutsut;
  private Integer kutsuttavienMaara;
  private Koekutsu kutsunKohde = Koekutsu.YLIN_TOIVE;
  private String kutsunKohdeAvain;

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

  public OsallistuminenTulos getOsallistuminenTulos() {
    return osallistuminenTulos;
  }

  public void setOsallistuminenTulos(OsallistuminenTulos osallistuminenTulos) {
    this.osallistuminenTulos = osallistuminenTulos;
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

  @Override
  public String toString() {
    return ToStringBuilder.reflectionToString(this);
  }
}
