package fi.vm.sade.valinta.kooste.external.resource.oppijanumerorekisteri.dto;

import static org.springframework.util.StringUtils.isEmpty;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import fi.vm.sade.valinta.kooste.external.resource.oppijanumerorekisteri.json.LocalDateDeserializer;
import fi.vm.sade.valinta.kooste.external.resource.oppijanumerorekisteri.json.LocalDateSerializer;
import jakarta.validation.constraints.AssertTrue;
import java.io.Serializable;
import java.time.LocalDate;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Set;

public class HenkiloPerustietoDto implements Serializable {
  private static final long serialVersionUID = -1263854768854256588L;

  private String oidHenkilo;
  private List<String> externalIds;
  private List<IdentificationDto> identifications;
  private String hetu;
  private String etunimet;
  private String kutsumanimi;
  private String sukunimi;

  @JsonSerialize(using = LocalDateSerializer.class)
  @JsonDeserialize(using = LocalDateDeserializer.class)
  private LocalDate syntymaaika;

  private KielisyysDto aidinkieli;
  private KielisyysDto asiointiKieli;
  private Set<KansalaisuusDto> kansalaisuus;
  private HenkiloTyyppi henkiloTyyppi;
  private String sukupuoli;
  private Date modified;

  public String getOidHenkilo() {
    return oidHenkilo;
  }

  public void setOidHenkilo(String oidHenkilo) {
    this.oidHenkilo = oidHenkilo;
  }

  public List<String> getExternalIds() {
    return externalIds;
  }

  public void setExternalIds(List<String> externalIds) {
    this.externalIds = externalIds;
  }

  public List<IdentificationDto> getIdentifications() {
    return identifications;
  }

  public void setIdentifications(List<IdentificationDto> identifications) {
    this.identifications = identifications;
  }

  public String getHetu() {
    return hetu;
  }

  public void setHetu(String hetu) {
    this.hetu = hetu;
  }

  public String getEtunimet() {
    return etunimet;
  }

  public void setEtunimet(String etunimet) {
    this.etunimet = etunimet;
  }

  public String getKutsumanimi() {
    return kutsumanimi;
  }

  public void setKutsumanimi(String kutsumanimi) {
    this.kutsumanimi = kutsumanimi;
  }

  public String getSukunimi() {
    return sukunimi;
  }

  public void setSukunimi(String sukunimi) {
    this.sukunimi = sukunimi;
  }

  public LocalDate getSyntymaaika() {
    return syntymaaika;
  }

  public void setSyntymaaika(LocalDate syntymaaika) {
    this.syntymaaika = syntymaaika;
  }

  public KielisyysDto getAidinkieli() {
    return aidinkieli;
  }

  public void setAidinkieli(KielisyysDto aidinkieli) {
    this.aidinkieli = aidinkieli;
  }

  public KielisyysDto getAsiointiKieli() {
    return asiointiKieli;
  }

  public void setAsiointiKieli(KielisyysDto asiointiKieli) {
    this.asiointiKieli = asiointiKieli;
  }

  public Set<KansalaisuusDto> getKansalaisuus() {
    return kansalaisuus == null ? Collections.emptySet() : kansalaisuus;
  }

  public void setKansalaisuus(Set<KansalaisuusDto> kansalaisuus) {
    this.kansalaisuus = kansalaisuus;
  }

  public HenkiloTyyppi getHenkiloTyyppi() {
    return henkiloTyyppi;
  }

  public void setHenkiloTyyppi(HenkiloTyyppi henkiloTyyppi) {
    this.henkiloTyyppi = henkiloTyyppi;
  }

  public String getSukupuoli() {
    return sukupuoli;
  }

  public void setSukupuoli(String sukupuoli) {
    this.sukupuoli = sukupuoli;
  }

  public Date getModified() {
    return modified;
  }

  public void setModified(Date modified) {
    this.modified = modified;
  }

  private boolean isFind() {
    return !isEmpty(getOidHenkilo());
  }

  @JsonIgnore
  @AssertTrue(message = "invalid.etunimet.empty")
  public boolean isEtunimetValidIfCreate() {
    return isFind() || !isEmpty(getEtunimet());
  }

  @JsonIgnore
  @AssertTrue(message = "invalid.kutsumanimi.empty")
  public boolean isKutsumanimiValidIfCreate() {
    return isFind() || !isEmpty(getKutsumanimi());
  }

  @JsonIgnore
  @AssertTrue(message = "invalid.sukunimi.empty")
  public boolean isSukunimiValidIfCreate() {
    return isFind() || !isEmpty(getSukunimi());
  }

  @JsonIgnore
  @AssertTrue(message = "invalid.henkilotyyppi.empty")
  public boolean isHenkilotyyppiValidIfCreate() {
    return isFind() || henkiloTyyppi != null;
  }

  @Override
  public String toString() {
    String overriddenHetu = hetu == null ? "null" : "***HETU***";
    return "HenkiloPerustietoDto{"
        + "oidHenkilo='"
        + oidHenkilo
        + '\''
        + ", externalIds="
        + externalIds
        + ", identifications="
        + identifications
        + ", hetu='"
        + overriddenHetu
        + '\''
        + ", etunimet='"
        + etunimet
        + '\''
        + ", kutsumanimi='"
        + kutsumanimi
        + '\''
        + ", sukunimi='"
        + sukunimi
        + '\''
        + ", syntymaaika="
        + syntymaaika
        + ", aidinkieli="
        + aidinkieli
        + ", asiointiKieli="
        + asiointiKieli
        + ", kansalaisuus="
        + kansalaisuus
        + ", henkiloTyyppi="
        + henkiloTyyppi
        + ", sukupuoli='"
        + sukupuoli
        + '\''
        + ", modified="
        + modified
        + '}';
  }
}
