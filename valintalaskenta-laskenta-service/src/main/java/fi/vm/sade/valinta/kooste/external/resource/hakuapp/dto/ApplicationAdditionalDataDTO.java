package fi.vm.sade.valinta.kooste.external.resource.hakuapp.dto;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import java.util.HashMap;
import java.util.Map;

@JsonSerialize(include = JsonSerialize.Inclusion.NON_EMPTY)
public class ApplicationAdditionalDataDTO {

  private String oid;
  private String personOid;
  private String firstNames;
  private String lastName;
  private Map<String, String> additionalData = new HashMap<>();

  public ApplicationAdditionalDataDTO() {}

  @JsonCreator
  public ApplicationAdditionalDataDTO(
      @JsonProperty("oid") String oid,
      @JsonProperty("personOid") String personOid,
      @JsonProperty("firstNames") String firstNames,
      @JsonProperty("lastName") String lastName,
      @JsonProperty("additionalData") Map<String, String> additionalData) {
    this.oid = oid;
    this.personOid = personOid;
    this.firstNames = firstNames;
    this.lastName = lastName;
    this.additionalData = additionalData;
  }

  public String getOid() {
    return oid;
  }

  public void setOid(String oid) {
    this.oid = oid;
  }

  public String getPersonOid() {
    return personOid;
  }

  public void setPersonOid(String personOid) {
    this.personOid = personOid;
  }

  public String getFirstNames() {
    return firstNames == null ? "" : firstNames;
  }

  public void setFirstNames(String firstNames) {
    this.firstNames = firstNames;
  }

  public String getLastName() {
    return lastName == null ? "" : lastName;
  }

  public void setLastName(String lastName) {
    this.lastName = lastName;
  }

  public Map<String, String> getAdditionalData() {
    if (additionalData == null) {
      return new HashMap<>();
    }
    return additionalData;
  }

  public void setAdditionalData(Map<String, String> additionalData) {
    this.additionalData = additionalData;
  }
}
