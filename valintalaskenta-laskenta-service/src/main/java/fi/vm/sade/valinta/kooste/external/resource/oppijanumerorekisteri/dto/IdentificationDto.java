package fi.vm.sade.valinta.kooste.external.resource.oppijanumerorekisteri.dto;

import java.io.Serializable;

public class IdentificationDto implements Serializable {
  private String idpEntityId;
  private String identifier;

  public static IdentificationDto of(String idpEntityId, String identifier) {
    IdentificationDto dto = new IdentificationDto();
    dto.setIdpEntityId(idpEntityId);
    dto.setIdentifier(identifier);
    return dto;
  }

  public String getIdpEntityId() {
    return idpEntityId;
  }

  public void setIdpEntityId(String idpEntityId) {
    this.idpEntityId = idpEntityId;
  }

  public String getIdentifier() {
    return identifier;
  }

  public void setIdentifier(String identifier) {
    this.identifier = identifier;
  }
}
