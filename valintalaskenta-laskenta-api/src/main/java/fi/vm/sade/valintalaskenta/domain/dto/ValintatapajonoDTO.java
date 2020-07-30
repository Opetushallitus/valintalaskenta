package fi.vm.sade.valintalaskenta.domain.dto;

import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.Tasasijasaanto;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.util.ArrayList;
import java.util.List;

@ApiModel(value = "valintalaskenta.domain.dto.ValintatapajonoDTO", description = "Valintatapajono")
public class ValintatapajonoDTO {

  @ApiModelProperty(value = "OID", required = true)
  private String valintatapajonooid;

  @ApiModelProperty(value = "Nimi", required = true)
  private String nimi;

  @ApiModelProperty(value = "Prioriteetti", required = true)
  private int prioriteetti;

  @ApiModelProperty(value = "Aloituspaikat", required = true)
  private int aloituspaikat;

  @ApiModelProperty(
      value =
          "Valintaperusteissa ylläpidettävä tieto siitä, onko jono tarkoitus joskus sijoitella vai ei",
      required = true)
  private boolean siirretaanSijoitteluun;

  @ApiModelProperty(value = "Tasasijasääntö", required = true)
  private Tasasijasaanto tasasijasaanto;

  @ApiModelProperty(value = "Onko varasijatäyttö käytössä", required = true)
  private Boolean eiVarasijatayttoa;

  @ApiModelProperty(
      value = "Hyväksytäänkö kaikki hyväksyttävissä olevat aloituspaikoista riippumatta",
      required = true)
  private Boolean kaikkiEhdonTayttavatHyvaksytaan = false;

  @ApiModelProperty(value = "Täytetäänkö poissaolevaksi ilmottautuneiden tilalle", required = true)
  private Boolean poissaOlevaTaytto = false;

  @ApiModelProperty(value = "Käytetäänkö valintalaskentaa", required = true)
  private Boolean kaytetaanValintalaskentaa = true;

  @ApiModelProperty(
      value =
          "Valintalaskennan tuloksissa ylläpidettävä tieto siitä, otetaanko jono mukaan sijoitteluajoon",
      required = false)
  private Boolean valmisSijoiteltavaksi = true;

  @ApiModelProperty(value = "Jonosijat", required = true)
  private List<JonosijaDTO> jonosijat = new ArrayList<JonosijaDTO>();

  @ApiModelProperty(value = "Onko valintatapajono aktiivinen", required = false)
  private Boolean aktiivinen = true;

  @ApiModelProperty(
      value = "Käytetäänkö kokonaispisteitä jonosijojen sijaan, jos laskenta ei ole käytössä",
      required = false)
  private Boolean kaytetaanKokonaispisteita;

  public ValintatapajonoDTO() {}

  public ValintatapajonoDTO(
      String valintatapajonooid,
      String nimi,
      int prioriteetti,
      int aloituspaikat,
      boolean siirretaanSijoitteluun,
      Tasasijasaanto tasasijasaanto,
      Boolean eiVarasijatayttoa,
      Boolean kaikkiEhdonTayttavatHyvaksytaan,
      Boolean poissaOlevaTaytto,
      Boolean kaytetaanValintalaskentaa,
      Boolean valmisSijoiteltavaksi,
      List<JonosijaDTO> jonosijat,
      Boolean aktiivinen) {
    this.valintatapajonooid = valintatapajonooid;
    this.nimi = nimi;
    this.prioriteetti = prioriteetti;
    this.aloituspaikat = aloituspaikat;
    this.siirretaanSijoitteluun = siirretaanSijoitteluun;
    this.tasasijasaanto = tasasijasaanto;
    this.eiVarasijatayttoa = eiVarasijatayttoa;
    this.kaikkiEhdonTayttavatHyvaksytaan = kaikkiEhdonTayttavatHyvaksytaan;
    this.poissaOlevaTaytto = poissaOlevaTaytto;
    this.kaytetaanValintalaskentaa = kaytetaanValintalaskentaa;
    this.valmisSijoiteltavaksi = valmisSijoiteltavaksi;
    this.jonosijat = jonosijat;
    this.aktiivinen = aktiivinen;
  }

  public String getNimi() {
    return nimi;
  }

  public void setNimi(String nimi) {
    this.nimi = nimi;
  }

  public boolean isSiirretaanSijoitteluun() {
    return siirretaanSijoitteluun;
  }

  public void setSiirretaanSijoitteluun(boolean siirretaanSijoitteluun) {
    this.siirretaanSijoitteluun = siirretaanSijoitteluun;
  }

  public int getPrioriteetti() {
    return prioriteetti;
  }

  public int getAloituspaikat() {
    return aloituspaikat;
  }

  public void setAloituspaikat(int aloituspaikat) {
    this.aloituspaikat = aloituspaikat;
  }

  public void setPrioriteetti(int prioriteetti) {
    this.prioriteetti = prioriteetti;
  }

  public String getOid() {
    return valintatapajonooid;
  }

  public void setOid(String oid) {
    this.valintatapajonooid = oid;
  }

  @Override
  public int hashCode() {
    return valintatapajonooid.hashCode();
  }

  public boolean equals(Object obj) {
    if (obj instanceof ValintatapajonoDTO) {
      ValintatapajonoDTO vtj = (ValintatapajonoDTO) obj;
      return this == vtj;
    }
    return false;
  }

  public Tasasijasaanto getTasasijasaanto() {
    return tasasijasaanto;
  }

  public void setTasasijasaanto(Tasasijasaanto tasasijasaanto) {
    this.tasasijasaanto = tasasijasaanto;
  }

  public List<JonosijaDTO> getJonosijat() {
    return jonosijat;
  }

  public void setJonosijat(List<JonosijaDTO> jonosijat) {
    this.jonosijat = jonosijat;
  }

  public Boolean getEiVarasijatayttoa() {
    return eiVarasijatayttoa;
  }

  public void setEiVarasijatayttoa(Boolean eiVarasijatayttoa) {
    this.eiVarasijatayttoa = eiVarasijatayttoa;
  }

  public Boolean getKaikkiEhdonTayttavatHyvaksytaan() {
    return kaikkiEhdonTayttavatHyvaksytaan;
  }

  public void setKaikkiEhdonTayttavatHyvaksytaan(Boolean kaikkiEhdonTayttavatHyvaksytaan) {
    this.kaikkiEhdonTayttavatHyvaksytaan = kaikkiEhdonTayttavatHyvaksytaan;
  }

  public Boolean getPoissaOlevaTaytto() {
    return poissaOlevaTaytto;
  }

  public void setPoissaOlevaTaytto(Boolean poissaOlevaTaytto) {
    this.poissaOlevaTaytto = poissaOlevaTaytto;
  }

  public Boolean getKaytetaanValintalaskentaa() {
    return kaytetaanValintalaskentaa;
  }

  public void setKaytetaanValintalaskentaa(Boolean kaytetaanValintalaskentaa) {
    this.kaytetaanValintalaskentaa = kaytetaanValintalaskentaa;
  }

  public String getValintatapajonooid() {
    return valintatapajonooid;
  }

  public void setValintatapajonooid(String valintatapajonooid) {
    this.valintatapajonooid = valintatapajonooid;
  }

  public Boolean getValmisSijoiteltavaksi() {
    return valmisSijoiteltavaksi;
  }

  public void setValmisSijoiteltavaksi(Boolean valmisSijoiteltavaksi) {
    this.valmisSijoiteltavaksi = valmisSijoiteltavaksi;
  }

  public Boolean getAktiivinen() {
    return aktiivinen;
  }

  public void setAktiivinen(Boolean aktiivinen) {
    this.aktiivinen = aktiivinen;
  }

  public Boolean getKaytetaanKokonaispisteita() {
    return kaytetaanKokonaispisteita;
  }

  public void setKaytetaanKokonaispisteita(Boolean kaytetaanKokonaispisteita) {
    this.kaytetaanKokonaispisteita = kaytetaanKokonaispisteita;
  }
}
