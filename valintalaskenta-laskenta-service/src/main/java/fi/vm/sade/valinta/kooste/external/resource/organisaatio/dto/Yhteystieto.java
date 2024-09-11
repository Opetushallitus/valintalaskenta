package fi.vm.sade.valinta.kooste.external.resource.organisaatio.dto;

public class Yhteystieto {
  private String osoiteTyyppi;
  private String tyyppi;
  private String kieli;
  private String id;
  private String yhteystietoOid;
  private String osoite;
  private String postinumeroUri;
  private String postitoimipaikka;
  private String ytjPaivitysPvm;
  private String coordinateType;
  private String lap;
  private String lng;
  private String osavaltio;
  private String extraRivi;
  private String maaUri;
  private String email;
  private String numero;

  public String getTyyppi() {
    return tyyppi;
  }

  public void setTyyppi(String tyyppi) {
    this.tyyppi = tyyppi;
  }

  public String getEmail() {
    return email;
  }

  public String getNumero() {
    return numero;
  }

  public void setEmail(String email) {
    this.email = email;
  }

  public void setNumero(String numero) {
    this.numero = numero;
  }

  public String getCoordinateType() {
    return coordinateType;
  }

  public String getExtraRivi() {
    return extraRivi;
  }

  public String getId() {
    return id;
  }

  public String getKieli() {
    return kieli;
  }

  public String getLap() {
    return lap;
  }

  public String getLng() {
    return lng;
  }

  public String getMaaUri() {
    return maaUri;
  }

  public String getOsavaltio() {
    return osavaltio;
  }

  public String getOsoite() {
    return osoite;
  }

  public String getOsoiteTyyppi() {
    return osoiteTyyppi;
  }

  public String getPostinumeroUri() {
    return postinumeroUri;
  }

  public String getPostitoimipaikka() {
    return postitoimipaikka;
  }

  public String getYhteystietoOid() {
    return yhteystietoOid;
  }

  public String getYtjPaivitysPvm() {
    return ytjPaivitysPvm;
  }

  public void setCoordinateType(String coordinateType) {
    this.coordinateType = coordinateType;
  }

  public void setExtraRivi(String extraRivi) {
    this.extraRivi = extraRivi;
  }

  public void setId(String id) {
    this.id = id;
  }

  public void setKieli(String kieli) {
    this.kieli = kieli;
  }

  public void setYhteystietoOid(String yhteystietoOid) {
    this.yhteystietoOid = yhteystietoOid;
  }

  public void setLap(String lap) {
    this.lap = lap;
  }

  public void setLng(String lng) {
    this.lng = lng;
  }

  public void setMaaUri(String maaUri) {
    this.maaUri = maaUri;
  }

  public void setOsavaltio(String osavaltio) {
    this.osavaltio = osavaltio;
  }

  public void setOsoite(String osoite) {
    this.osoite = osoite;
  }

  public void setOsoiteTyyppi(String osoiteTyyppi) {
    this.osoiteTyyppi = osoiteTyyppi;
  }

  public void setPostinumeroUri(String postinumeroUri) {
    this.postinumeroUri = postinumeroUri;
  }

  public void setPostitoimipaikka(String postitoimipaikka) {
    this.postitoimipaikka = postitoimipaikka;
  }

  public void setYtjPaivitysPvm(String ytjPaivitysPvm) {
    this.ytjPaivitysPvm = ytjPaivitysPvm;
  }
}
