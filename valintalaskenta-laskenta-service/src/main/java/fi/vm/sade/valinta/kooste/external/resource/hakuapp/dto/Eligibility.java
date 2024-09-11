package fi.vm.sade.valinta.kooste.external.resource.hakuapp.dto;

import fi.vm.sade.valinta.kooste.erillishaku.excel.Maksuvelvollisuus;

public class Eligibility {
  private String aoId;
  private String status;
  private String source;
  private String maksuvelvollisuus;

  public Eligibility() {
    this.aoId = null;
    this.status = null;
    this.source = null;
    this.maksuvelvollisuus = null;
  }

  public Eligibility(String aoId, String status, String source, String maksuvelvollisuus) {
    this.aoId = aoId;
    this.status = status;
    this.source = source;
    this.maksuvelvollisuus = maksuvelvollisuus;
  }

  public String getAoId() {
    return aoId;
  }

  public String getSource() {
    return source;
  }

  public String getStatus() {
    return status;
  }

  public String getParsedEligibilityStatus() {
    return status.equals("AUTOMATICALLY_CHECKED_ELIGIBLE") ? "ELIGIBLE" : status;
  }

  public String getMaksuvelvollisuus() {
    if (maksuvelvollisuus == null) {
      return Maksuvelvollisuus.NOT_CHECKED;
    }
    return maksuvelvollisuus;
  }
}
