package fi.vm.sade.valintalaskenta.domain.valinta;

import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.Tasasijasaanto;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import javax.persistence.*;

@Entity
@Table(name = "valintatapajono")
public class ValintatapajonoMigrationDTO {
  @Id
  @GeneratedValue(strategy = GenerationType.AUTO)
  private Long id;

  private int schemaVersion;

  //@Indexed
  private String valintatapajonoOid;

  private String nimi;

  private int prioriteetti;

  private int aloituspaikat;

  private boolean siirretaanSijoitteluun;

  private Tasasijasaanto tasasijasaanto;

  private Boolean eiVarasijatayttoa;

  private Boolean kaikkiEhdonTayttavatHyvaksytaan;

  private Boolean kaytetaanValintalaskentaa;

  private Boolean poissaOlevaTaytto;

  private Boolean valmisSijoiteltavaksi = true;

  private Boolean kaytetaanKokonaispisteita;

  @Embedded private List<JonosijaMigrationDTO> jonosijat;

  private Long sijoitteluajoId;

  public Long getId() {
    return this.id;
  }

  public int getSchemaVersion() {
    return schemaVersion;
  }

  public void setSchemaVersion(int schemaVersion) {
    this.schemaVersion = schemaVersion;
  }

  public String getValintatapajonoOid() {
    return valintatapajonoOid;
  }

  public void setValintatapajonoOid(String valintatapajonoOid) {
    this.valintatapajonoOid = valintatapajonoOid;
  }

  public String getNimi() {
    return nimi;
  }

  public void setNimi(String nimi) {
    this.nimi = nimi;
  }

  public int getPrioriteetti() {
    return prioriteetti;
  }

  public void setPrioriteetti(int prioriteetti) {
    this.prioriteetti = prioriteetti;
  }

  public int getAloituspaikat() {
    return aloituspaikat;
  }

  public void setAloituspaikat(int aloituspaikat) {
    this.aloituspaikat = aloituspaikat;
  }

  public boolean isSiirretaanSijoitteluun() {
    return siirretaanSijoitteluun;
  }

  public void setSiirretaanSijoitteluun(boolean siirretaanSijoitteluun) {
    this.siirretaanSijoitteluun = siirretaanSijoitteluun;
  }

  public Tasasijasaanto getTasasijasaanto() {
    return tasasijasaanto;
  }

  public void setTasasijasaanto(Tasasijasaanto tasasijasaanto) {
    this.tasasijasaanto = tasasijasaanto;
  }

  public Boolean getEiVarasijatayttoa() {
    return eiVarasijatayttoa;
  }

  public void setEiVarasijatayttoa(Boolean eiVarasijatayttoa) {
    this.eiVarasijatayttoa = eiVarasijatayttoa;
  }

  public void setJonosijat(List<JonosijaMigrationDTO> jonosijat) {
    this.jonosijat = jonosijat;
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

  public Boolean getKaytetaanKokonaispisteita() {
    return kaytetaanKokonaispisteita;
  }

  public void setKaytetaanKokonaispisteita(Boolean kaytetaanKokonaispisteita) {
    this.kaytetaanKokonaispisteita = kaytetaanKokonaispisteita;
  }

  public Boolean getValmisSijoiteltavaksi() {
    return valmisSijoiteltavaksi;
  }

  public void setValmisSijoiteltavaksi(Boolean valmisSijoiteltavaksi) {
    this.valmisSijoiteltavaksi = valmisSijoiteltavaksi;
  }

  public Long getSijoitteluajoId() {
    return sijoitteluajoId;
  }

  public void setSijoitteluajoId(Long sijoitteluajoId) {
    this.sijoitteluajoId = sijoitteluajoId;
  }

  public Valintatapajono migrate() {
    Valintatapajono jono = new Valintatapajono();
    jono.setId(id);
    jono.setSchemaVersion(Valintatapajono.CURRENT_SCHEMA_VERSION);
    jono.setValintatapajonoOid(valintatapajonoOid);
    jono.setNimi(nimi);
    jono.setPrioriteetti(prioriteetti);
    jono.setAloituspaikat(aloituspaikat);
    jono.setSiirretaanSijoitteluun(siirretaanSijoitteluun);
    jono.setTasasijasaanto(tasasijasaanto);
    jono.setEiVarasijatayttoa(eiVarasijatayttoa);
    jono.setKaikkiEhdonTayttavatHyvaksytaan(kaikkiEhdonTayttavatHyvaksytaan);
    jono.setKaytetaanValintalaskentaa(kaytetaanValintalaskentaa);
    jono.setPoissaOlevaTaytto(poissaOlevaTaytto);
    jono.setValmisSijoiteltavaksi(valmisSijoiteltavaksi);
    jono.setKaytetaanKokonaispisteita(kaytetaanKokonaispisteita);
    if (null == jonosijat) {
      jono.setJonosijat(new ArrayList<>());
    } else {
      jono.setJonosijat(
          jonosijat.stream().map(jonosija -> jonosija.migrate()).collect(Collectors.toList()));
    }
    jono.setSijoitteluajoId(sijoitteluajoId);
    return jono;
  }
}
