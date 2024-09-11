package fi.vm.sade.valinta.kooste.external.resource.ataru.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;

@JsonIgnoreProperties(ignoreUnknown = true)
public class AtaruHakemus {

  private String hakemusOid;
  private String personOid;
  private String hakuOid;
  private String asiointikieli;
  private List<AtaruHakutoive> hakutoiveet;
  private Map<String, String> maksuvelvollisuus;
  private Map<String, String> keyValues;

  public AtaruHakemus() {}

  public AtaruHakemus(
      String hakemusOid,
      String personOid,
      String hakuOid,
      List<AtaruHakutoive> hakutoiveet,
      Map<String, String> maksuvelvollisuus,
      String asiointikieli,
      Map<String, String> keyValues) {
    this.hakemusOid = hakemusOid;
    this.personOid = personOid;
    this.hakuOid = hakuOid;
    this.asiointikieli = asiointikieli;
    this.hakutoiveet = hakutoiveet;
    this.maksuvelvollisuus = maksuvelvollisuus;
    this.keyValues = keyValues;
  }

  public String getAsiointikieli() {
    return asiointikieli;
  }

  public void setAsiointikieli(String asiointikieli) {
    this.asiointikieli = asiointikieli;
  }

  public String getHakemusOid() {
    return hakemusOid;
  }

  public void setHakemusOid(String hakemusOid) {
    this.hakemusOid = hakemusOid;
  }

  public String getPersonOid() {
    return personOid;
  }

  public void setPersonOid(String personOid) {
    this.personOid = personOid;
  }

  public String getHakuOid() {
    return hakuOid;
  }

  public void setHakuOid(String hakuOid) {
    this.hakuOid = hakuOid;
  }

  public List<AtaruHakutoive> getHakutoiveet() {
    return hakutoiveet;
  }

  public void setHakutoiveet(List<AtaruHakutoive> hakutoiveet) {
    this.hakutoiveet = hakutoiveet;
  }

  public Map<String, String> getMaksuvelvollisuus() {
    return maksuvelvollisuus;
  }

  public void setMaksuvelvollisuus(Map<String, String> maksuvelvollisuus) {
    this.maksuvelvollisuus = maksuvelvollisuus;
  }

  public Map<String, String> getKeyValues() {
    return keyValues;
  }

  public void setKeyValues(Map<String, String> keyValues) {
    this.keyValues = keyValues;
  }

  public boolean hasHetu() {
    return StringUtils.isNotEmpty(keyValues.get("ssn"));
  }

  @Override
  public String toString() {
    return "AtaruHakemus{"
        + "hakemusOid='"
        + hakemusOid
        + '\''
        + ", personOid='"
        + personOid
        + '\''
        + ", hakuOid='"
        + hakuOid
        + '\''
        + ", asiointikieli='"
        + asiointikieli
        + '\''
        + ", hakutoiveet="
        + hakutoiveet
        + ", maksuvelvollisuus="
        + maksuvelvollisuus
        + ", keyValues="
        + keyValues
        + '}';
  }
}
