package fi.vm.sade.valintalaskenta.ovara.ajastus;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.sql.Timestamp;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.util.UUID;
import javax.persistence.Column;
import javax.persistence.Id;

public class SiirtotiedostoProsessi {
  @Id
  @Column(name = "execution_uuid")
  private String executionUuid;

  @Column(name = "window_start", nullable = false)
  private OffsetDateTime windowStart;

  @Column(name = "window_end", nullable = false)
  private OffsetDateTime windowEnd;

  @Column(name = "run_start", nullable = false)
  private OffsetDateTime runStart;

  @Column(name = "run_end")
  private OffsetDateTime runEnd;

  @Column(name = "info")
  private String info;

  @Column(name = "success")
  private Boolean success;

  @Column(name = "error_message")
  private String errorMessage;

  ObjectMapper mapper = new ObjectMapper();

  public SiirtotiedostoProsessi(
      String executionUuid,
      OffsetDateTime windowStart,
      OffsetDateTime windodwEnd,
      OffsetDateTime runStart,
      OffsetDateTime runEnd,
      String info,
      Boolean success,
      String errorMessage) {
    this.executionUuid = executionUuid;
    this.windowStart = windowStart;
    this.windowEnd = windodwEnd;
    this.runStart = runStart;
    this.runEnd = runEnd;
    this.info = info;
    this.success = success;
    this.errorMessage = errorMessage;
  }

  public SiirtotiedostoProsessi(Object[] result) {
    this.executionUuid = (String) result[0];
    this.windowStart = ((Timestamp) result[1]).toInstant().atOffset(ZoneOffset.UTC);
    this.windowEnd = ((Timestamp) result[2]).toInstant().atOffset(ZoneOffset.UTC);
    this.runStart = ((Timestamp) result[3]).toInstant().atOffset(ZoneOffset.UTC);
    this.runEnd = ((Timestamp) result[4]).toInstant().atOffset(ZoneOffset.UTC);
    this.info = (String) result[5];
    this.success = (Boolean) result[6];
    this.errorMessage = (String) result[7];
  }

  public SiirtotiedostoProsessi() {}

  public String getErrorMessage() {
    return errorMessage;
  }

  public void setErrorMessage(String errorMessage) {
    this.errorMessage = errorMessage;
  }

  public Boolean getSuccess() {
    return success;
  }

  public void setSuccess(Boolean success) {
    this.success = success;
  }

  public String getInfo() {
    return info;
  }

  public void setInfo(String info) {
    this.info = info;
  }

  public OffsetDateTime getWindowEnd() {
    return windowEnd;
  }

  public void setWindowEnd(OffsetDateTime windowEnd) {
    this.windowEnd = windowEnd;
  }

  public OffsetDateTime getWindowStart() {
    return windowStart;
  }

  public void setWindowStart(OffsetDateTime windowStart) {
    this.windowStart = windowStart;
  }

  public String getExecutionUuid() {
    return executionUuid;
  }

  public void setExecutionUuid(String executionUuid) {
    this.executionUuid = executionUuid;
  }

  public OffsetDateTime getRunEnd() {
    return runEnd;
  }

  public void setRunEnd(OffsetDateTime runEnd) {
    this.runEnd = runEnd;
  }

  public OffsetDateTime getRunStart() {
    return runStart;
  }

  public void setRunStart(OffsetDateTime runStart) {
    this.runStart = runStart;
  }

  public SiirtotiedostoProsessi createNewProcessBasedOnThis() {
    return new SiirtotiedostoProsessi(
        UUID.randomUUID().toString(),
        this.windowEnd,
        OffsetDateTime.now(),
        OffsetDateTime.now(),
        null,
        null,
        null,
        "");
  }

  @Override
  public String toString() {
    return "SiirtotiedostoProsessi{"
        + "executionUuid="
        + executionUuid
        + ", windowStart="
        + windowStart
        + ", windowEnd="
        + windowEnd
        + ", runStart="
        + runStart
        + ", runEnd="
        + runEnd
        + ", info="
        + info
        + ", success="
        + success
        + ", errorMessage='"
        + errorMessage
        + '\''
        + '}';
  }
}
