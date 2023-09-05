package fi.vm.sade.valintalaskenta.domain.valinta;

import java.util.Date;

public class LogEntry {
  private String id;

  private Date luotu;
  private String muokkaaja;
  private String muutos;
  private String selite;

  public Date getLuotu() {
    return luotu;
  }

  public void setLuotu(Date luotu) {
    this.luotu = luotu;
  }

  public String getMuokkaaja() {
    return muokkaaja;
  }

  public void setMuokkaaja(String muokkaaja) {
    this.muokkaaja = muokkaaja;
  }

  public String getSelite() {
    return selite;
  }

  public void setSelite(String selite) {
    this.selite = selite;
  }

  public String getId() {
    return id;
  }

  public void setId(String id) {
    this.id = id;
  }

  public String getMuutos() {
    return muutos;
  }

  public void setMuutos(String muutos) {
    this.muutos = muutos;
  }
}
