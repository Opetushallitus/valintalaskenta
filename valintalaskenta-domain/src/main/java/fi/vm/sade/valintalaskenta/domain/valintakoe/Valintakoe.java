package fi.vm.sade.valintalaskenta.domain.valintakoe;

import fi.vm.sade.service.valintaperusteet.dto.model.Koekutsu;
import org.apache.commons.lang3.builder.ToStringBuilder;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.ManyToOne;
import java.util.UUID;

@Entity(name = "Valintakoe")
public class Valintakoe {

  @Id
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

  @Override
  public String toString() {
    return ToStringBuilder.reflectionToString(this);
  }
}
