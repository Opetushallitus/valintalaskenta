package fi.vm.sade.valintalaskenta.domain.dto;

import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.Tasasijasaanto;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.ArrayList;
import java.util.List;

@Schema(name = "valintalaskenta.ValintatapajonoDTO", description = "Valintatapajono")
public class ValintatapajonoDTO {

  @Schema(description = "OID", required = true)
  private String valintatapajonooid;

  @Schema(description = "Nimi", required = true)
  private String nimi;

  @Schema(description = "Prioriteetti", required = true)
  private int prioriteetti;

  @Schema(description = "Aloituspaikat", required = true)
  private int aloituspaikat;

  @Schema(
      description =
          "Valintaperusteissa ylläpidettävä tieto siitä, onko jono tarkoitus joskus sijoitella vai ei",
      required = true)
  private boolean siirretaanSijoitteluun;

  @Schema(description = "Tasasijasääntö", required = true)
  private Tasasijasaanto tasasijasaanto;

  @Schema(description = "Onko varasijatäyttö käytössä", required = true)
  private Boolean eiVarasijatayttoa;

  @Schema(
      description = "Hyväksytäänkö kaikki hyväksyttävissä olevat aloituspaikoista riippumatta",
      required = true)
  private Boolean kaikkiEhdonTayttavatHyvaksytaan = false;

  @Schema(description = "Täytetäänkö poissaolevaksi ilmottautuneiden tilalle", required = true)
  private Boolean poissaOlevaTaytto = false;

  @Schema(description = "Käytetäänkö valintalaskentaa", required = true)
  private Boolean kaytetaanValintalaskentaa = true;

  @Schema(
      title =
          "Valintalaskennan tuloksissa ylläpidettävä tieto siitä, otetaanko jono mukaan sijoitteluajoon",
      required = false)
  private Boolean valmisSijoiteltavaksi = true;

  @Schema(description = "Jonosijat", required = true)
  private List<JonosijaDTO> jonosijat = new ArrayList<JonosijaDTO>();

  @Schema(description = "Onko valintatapajono aktiivinen", required = false)
  private Boolean aktiivinen = true;

  @Schema(
      title = "Käytetäänkö kokonaispisteitä jonosijojen sijaan, jos laskenta ei ole käytössä",
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
