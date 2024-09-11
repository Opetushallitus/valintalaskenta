package fi.vm.sade.valinta.kooste.hakemus.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;

public class ApplicationOidsAndReason {
  public final List<String> applicationOids;
  public final String reason;

  public ApplicationOidsAndReason(
      @JsonProperty("applicationOids") List<String> applicationOids,
      @JsonProperty("reason") String reason) {
    this.applicationOids = applicationOids;
    this.reason = reason;
  }

  @Override
  public String toString() {
    return "ApplicationOidsAndReason{applicationOids="
        + applicationOids
        + ", reason='"
        + reason
        + "'}";
  }
}
