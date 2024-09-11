package fi.vm.sade.valinta.kooste.external.resource.suoritusrekisteri.dto;

import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Suoritus {

  private static final Logger LOG = LoggerFactory.getLogger(Suoritus.class);

  private String id;
  private String komo;
  private String myontaja;
  private String tila;
  private String valmistuminen;
  private String henkiloOid;
  private String yksilollistaminen;
  private String suoritusKieli;
  private String source;
  private Map<String, String> lahdeArvot;
  private boolean vahvistettu;

  public void setHenkiloOid(String henkiloOid) {
    this.henkiloOid = henkiloOid;
  }

  public void setId(String id) {
    this.id = id;
  }

  public void setKomo(String komo) {
    this.komo = komo;
  }

  public void setMyontaja(String myontaja) {
    this.myontaja = myontaja;
  }

  public void setSource(String source) {
    this.source = source;
  }

  public void setSuoritusKieli(String suoritusKieli) {
    this.suoritusKieli = suoritusKieli;
  }

  public void setTila(String tila) {
    this.tila = tila;
  }

  public void setValmistuminen(String valmistuminen) {
    this.valmistuminen = valmistuminen;
  }

  public void setYksilollistaminen(String yksilollistaminen) {
    this.yksilollistaminen = yksilollistaminen;
  }

  public void setLahdeArvot(Map<String, String> lahdeArvot) {
    this.lahdeArvot = lahdeArvot;
  }

  public String getHenkiloOid() {
    return henkiloOid;
  }

  public String getId() {
    return id;
  }

  public String getKomo() {
    return komo;
  }

  public String getMyontaja() {
    return myontaja;
  }

  public String getSource() {
    return source;
  }

  public String getSuoritusKieli() {
    return suoritusKieli;
  }

  public String getTila() {
    return tila;
  }

  public String getValmistuminen() {
    return valmistuminen;
  }

  public String getYksilollistaminen() {
    return yksilollistaminen;
  }

  public Map<String, String> getLahdeArvot() {
    return lahdeArvot;
  }

  public boolean isVahvistettu() {
    return vahvistettu;
  }

  public void setVahvistettu(boolean vahvistettu) {
    this.vahvistettu = vahvistettu;
  }

  public boolean isYksilollistettyMaAi() {
    return !"KESKEYTYNYT".equals(this.tila)
        && this.lahdeArvot.getOrDefault("yksilollistetty_ma_ai", "false").equals("true");
  }

  @Override
  public String toString() {
    return "Suoritus{"
        + "id='"
        + id
        + '\''
        + ", komo='"
        + komo
        + '\''
        + ", myontaja='"
        + myontaja
        + '\''
        + ", tila='"
        + tila
        + '\''
        + ", valmistuminen='"
        + valmistuminen
        + '\''
        + ", henkiloOid='"
        + henkiloOid
        + '\''
        + ", yksilollistaminen='"
        + yksilollistaminen
        + '\''
        + ", suoritusKieli='"
        + suoritusKieli
        + '\''
        + ", source='"
        + source
        + '\''
        + ", vahvistettu="
        + vahvistettu
        + '}';
  }
}
