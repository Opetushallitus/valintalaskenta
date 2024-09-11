package fi.vm.sade.valinta.kooste.external.resource.tarjonta.dto;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class HakukohdeValintaperusteetDTO {

  private String tarjoajaOid;
  private Map<String, String> tarjoajaNimi = new HashMap<>();

  private Map<String, String> hakukohdeNimi = new HashMap<>();

  private int hakuVuosi;
  private Map<String, String> hakuKausi = new HashMap<>();

  private String hakuOid;
  private String hakuKohdejoukkoUri;

  private List<String> opetuskielet = new ArrayList<>();
  private String hakukohdeNimiUri;

  private int valintojenAloituspaikatLkm;

  private List<ValintakoeDTO> valintakokeet = new ArrayList<>();

  private Map<String, String> painokertoimet = new HashMap<>();

  private String paasykoeTunniste;
  private String lisanayttoTunniste;
  private String lisapisteTunniste;
  private String kielikoeTunniste;
  private String urheilijaLisapisteTunniste;

  private BigDecimal painotettuKeskiarvoHylkaysMin;
  private BigDecimal painotettuKeskiarvoHylkaysMax;

  private BigDecimal paasykoeMin;
  private BigDecimal paasykoeMax;

  private BigDecimal paasykoeHylkaysMin;
  private BigDecimal paasykoeHylkaysMax;

  private BigDecimal lisanayttoMin;
  private BigDecimal lisanayttoMax;

  private BigDecimal lisanayttoHylkaysMin;
  private BigDecimal lisanayttoHylkaysMax;

  private BigDecimal lisapisteMin;
  private BigDecimal lisapisteMax;

  private BigDecimal lisapisteHylkaysMin;
  private BigDecimal lisapisteHylkaysMax;

  private BigDecimal hylkaysMin;
  private BigDecimal hylkaysMax;

  private String tila;

  public String getUrheilijaLisapisteTunniste() {
    return urheilijaLisapisteTunniste;
  }

  public void setUrheilijaLisapisteTunniste(String urheilijaLisapisteTunniste) {
    this.urheilijaLisapisteTunniste = urheilijaLisapisteTunniste;
  }

  public String getTila() {
    return tila;
  }

  public void setTila(String tila) {
    this.tila = tila;
  }

  public BigDecimal getPaasykoeMin() {
    return paasykoeMin;
  }

  public void setPaasykoeMin(BigDecimal paasykoeMin) {
    this.paasykoeMin = paasykoeMin;
  }

  public BigDecimal getPaasykoeMax() {
    return paasykoeMax;
  }

  public void setPaasykoeMax(BigDecimal paasykoeMax) {
    this.paasykoeMax = paasykoeMax;
  }

  public BigDecimal getLisanayttoMin() {
    return lisanayttoMin;
  }

  public void setLisanayttoMin(BigDecimal lisanayttoMin) {
    this.lisanayttoMin = lisanayttoMin;
  }

  public BigDecimal getLisanayttoMax() {
    return lisanayttoMax;
  }

  public void setLisanayttoMax(BigDecimal lisanayttoMax) {
    this.lisanayttoMax = lisanayttoMax;
  }

  public BigDecimal getLisapisteMin() {
    return lisapisteMin;
  }

  public void setLisapisteMin(BigDecimal lisapisteMin) {
    this.lisapisteMin = lisapisteMin;
  }

  public BigDecimal getLisapisteMax() {
    return lisapisteMax;
  }

  public void setLisapisteMax(BigDecimal lisapisteMax) {
    this.lisapisteMax = lisapisteMax;
  }

  public BigDecimal getPaasykoeHylkaysMin() {
    return paasykoeHylkaysMin;
  }

  public void setPaasykoeHylkaysMin(BigDecimal paasykoeHylkaysMin) {
    this.paasykoeHylkaysMin = paasykoeHylkaysMin;
  }

  public BigDecimal getPaasykoeHylkaysMax() {
    return paasykoeHylkaysMax;
  }

  public void setPaasykoeHylkaysMax(BigDecimal paasykoeHylkaysMax) {
    this.paasykoeHylkaysMax = paasykoeHylkaysMax;
  }

  public BigDecimal getLisanayttoHylkaysMin() {
    return lisanayttoHylkaysMin;
  }

  public void setLisanayttoHylkaysMin(BigDecimal lisanayttoHylkaysMin) {
    this.lisanayttoHylkaysMin = lisanayttoHylkaysMin;
  }

  public BigDecimal getLisanayttoHylkaysMax() {
    return lisanayttoHylkaysMax;
  }

  public void setLisanayttoHylkaysMax(BigDecimal lisanayttoHylkaysMax) {
    this.lisanayttoHylkaysMax = lisanayttoHylkaysMax;
  }

  public BigDecimal getLisapisteHylkaysMin() {
    return lisapisteHylkaysMin;
  }

  public void setLisapisteHylkaysMin(BigDecimal lisapisteHylkaysMin) {
    this.lisapisteHylkaysMin = lisapisteHylkaysMin;
  }

  public BigDecimal getLisapisteHylkaysMax() {
    return lisapisteHylkaysMax;
  }

  public void setLisapisteHylkaysMax(BigDecimal lisapisteHylkaysMax) {
    this.lisapisteHylkaysMax = lisapisteHylkaysMax;
  }

  public BigDecimal getPainotettuKeskiarvoHylkaysMin() {
    return painotettuKeskiarvoHylkaysMin;
  }

  public void setPainotettuKeskiarvoHylkaysMin(BigDecimal painotettuKeskiarvoHylkaysMin) {
    this.painotettuKeskiarvoHylkaysMin = painotettuKeskiarvoHylkaysMin;
  }

  public BigDecimal getPainotettuKeskiarvoHylkaysMax() {
    return painotettuKeskiarvoHylkaysMax;
  }

  public void setPainotettuKeskiarvoHylkaysMax(BigDecimal painotettuKeskiarvoHylkaysMax) {
    this.painotettuKeskiarvoHylkaysMax = painotettuKeskiarvoHylkaysMax;
  }

  public BigDecimal getHylkaysMin() {
    return hylkaysMin;
  }

  public void setHylkaysMin(BigDecimal hylkaysMin) {
    this.hylkaysMin = hylkaysMin;
  }

  public BigDecimal getHylkaysMax() {
    return hylkaysMax;
  }

  public void setHylkaysMax(BigDecimal hylkaysMax) {
    this.hylkaysMax = hylkaysMax;
  }

  public String getTarjoajaOid() {
    return tarjoajaOid;
  }

  public void setTarjoajaOid(String tarjoajaOid) {
    this.tarjoajaOid = tarjoajaOid;
  }

  public Map<String, String> getTarjoajaNimi() {
    return tarjoajaNimi;
  }

  public void setTarjoajaNimi(Map<String, String> tarjoajaNimi) {
    this.tarjoajaNimi = tarjoajaNimi;
  }

  public Map<String, String> getHakukohdeNimi() {
    return hakukohdeNimi;
  }

  public void setHakukohdeNimi(Map<String, String> hakukohdeNimi) {
    this.hakukohdeNimi = hakukohdeNimi;
  }

  public int getHakuVuosi() {
    return hakuVuosi;
  }

  public void setHakuVuosi(int hakuVuosi) {
    this.hakuVuosi = hakuVuosi;
  }

  public Map<String, String> getHakuKausi() {
    return hakuKausi;
  }

  public void setHakuKausi(Map<String, String> hakuKausi) {
    this.hakuKausi = hakuKausi;
  }

  public String getHakuOid() {
    return hakuOid;
  }

  public void setHakuOid(String hakuOid) {
    this.hakuOid = hakuOid;
  }

  public List<String> getOpetuskielet() {
    return opetuskielet;
  }

  public void setOpetuskielet(List<String> opetuskielet) {
    this.opetuskielet = opetuskielet;
  }

  public String getHakukohdeNimiUri() {
    return hakukohdeNimiUri;
  }

  public void setHakukohdeNimiUri(String hakukohdeNimiUri) {
    this.hakukohdeNimiUri = hakukohdeNimiUri;
  }

  public int getValintojenAloituspaikatLkm() {
    return valintojenAloituspaikatLkm;
  }

  public void setValintojenAloituspaikatLkm(int valintojenAloituspaikatLkm) {
    this.valintojenAloituspaikatLkm = valintojenAloituspaikatLkm;
  }

  public List<ValintakoeDTO> getValintakokeet() {
    return valintakokeet;
  }

  public void setValintakokeet(List<ValintakoeDTO> valintakokeet) {
    this.valintakokeet = valintakokeet;
  }

  public Map<String, String> getPainokertoimet() {
    return painokertoimet;
  }

  public void setPainokertoimet(Map<String, String> painokertoimet) {
    this.painokertoimet = painokertoimet;
  }

  public String getPaasykoeTunniste() {
    return paasykoeTunniste;
  }

  public void setPaasykoeTunniste(String paasykoeTunniste) {
    this.paasykoeTunniste = paasykoeTunniste;
  }

  public String getLisanayttoTunniste() {
    return lisanayttoTunniste;
  }

  public void setLisanayttoTunniste(String lisanayttoTunniste) {
    this.lisanayttoTunniste = lisanayttoTunniste;
  }

  public String getLisapisteTunniste() {
    return lisapisteTunniste;
  }

  public void setLisapisteTunniste(String lisapisteTunniste) {
    this.lisapisteTunniste = lisapisteTunniste;
  }

  public String getKielikoeTunniste() {
    return kielikoeTunniste;
  }

  public void setKielikoeTunniste(String kielikoeTunniste) {
    this.kielikoeTunniste = kielikoeTunniste;
  }

  public String getHakuKohdejoukkoUri() {
    return hakuKohdejoukkoUri;
  }

  public void setHakuKohdejoukkoUri(String hakuKohdejoukkoUri) {
    this.hakuKohdejoukkoUri = hakuKohdejoukkoUri;
  }
}
