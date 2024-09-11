package fi.vm.sade.valinta.kooste.external.resource.hakuapp.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * TODO: Refaktoroi pois haku-paketin alta. Tämä dto ei liity haku-palveluun vaan hakemus-palveluun.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class Hakemus {
  private String type;
  private String applicationSystemId;
  private Answers answers = new Answers();
  private Map<String, String> additionalInfo = new HashMap<String, String>();
  private List<Eligibility> preferenceEligibilities;
  private String oid;
  private String state;
  private String personOid;

  public Hakemus() {}

  public Hakemus(
      final String type,
      final String applicationSystemId,
      final Answers answers,
      final Map<String, String> additionalInfo,
      final List<Eligibility> preferenceEligibilities,
      final String oid,
      final String state,
      final String personOid) {
    this.type = type;
    this.applicationSystemId = applicationSystemId;
    this.answers = answers;
    this.additionalInfo = additionalInfo;
    this.preferenceEligibilities = preferenceEligibilities;
    this.oid = oid;
    this.state = state;
    this.personOid = personOid;
  }

  public String getType() {
    return type;
  }

  public List<Eligibility> getPreferenceEligibilities() {
    return preferenceEligibilities;
  }

  public void setType(String type) {
    this.type = type;
  }

  public String getApplicationSystemId() {
    return applicationSystemId;
  }

  public void setApplicationSystemId(String applicationSystemId) {
    this.applicationSystemId = applicationSystemId;
  }

  public Answers getAnswers() {
    return answers;
  }

  public void setAnswers(Answers answers) {
    this.answers = answers;
  }

  public String getOid() {
    return oid;
  }

  public void setOid(String oid) {
    this.oid = oid;
  }

  public String getState() {
    return state;
  }

  public void setState(String state) {
    this.state = state;
  }

  public String getPersonOid() {
    return personOid;
  }

  public void setPersonOid(String personOid) {
    this.personOid = personOid;
  }

  public Map<String, String> getAdditionalInfo() {
    return additionalInfo;
  }

  public void setAdditionalInfo(Map<String, String> additionalInfo) {
    this.additionalInfo = additionalInfo;
  }

  @Override
  public int hashCode() {
    return Optional.ofNullable(oid).orElse("").hashCode();
  }

  @Override
  public boolean equals(Object obj) {
    if (obj == null) {
      return false;
    } else if (this == obj) {
      return true;
    } else if (obj instanceof Hakemus) {
      return this.oid.equals(((Hakemus) obj).oid);
    } else {
      return false;
    }
  }
}
