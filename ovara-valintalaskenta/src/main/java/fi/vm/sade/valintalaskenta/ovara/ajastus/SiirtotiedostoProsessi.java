package fi.vm.sade.valintalaskenta.ovara.ajastus;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.sql.Timestamp;
import java.util.UUID;
import javax.persistence.Column;
import javax.persistence.Id;

public class SiirtotiedostoProsessi {
  @Id
  @Column(name = "execution_uuid")
  private String executionUuid;

  @Column(name = "window_start", nullable = false)
  private Timestamp windowStart;

  @Column(name = "window_end", nullable = false)
  private Timestamp windowEnd;

  @Column(name = "run_start", nullable = false)
  private Timestamp runStart;

  @Column(name = "run_end")
  private Timestamp runEnd;

  @Column(name = "info")
  private String info;

  @Column(name = "success")
  private Boolean success;

  @Column(name = "error_message")
  private String errorMessage;

  ObjectMapper mapper = new ObjectMapper();

  public SiirtotiedostoProsessi(
      String executionUuid,
      Timestamp windowStart,
      Timestamp windodwEnd,
      Timestamp runStart,
      Timestamp runEnd,
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

  public Timestamp getWindowEnd() {
    return windowEnd;
  }

  public void setWindowEnd(Timestamp windowEnd) {
    this.windowEnd = windowEnd;
  }

  public Timestamp getWindowStart() {
    return windowStart;
  }

  public void setWindowStart(Timestamp windowStart) {
    this.windowStart = windowStart;
  }

  public String getExecutionUuid() {
    return executionUuid;
  }

  public void setExecutionUuid(String executionUuid) {
    this.executionUuid = executionUuid;
  }

  public Timestamp getRunEnd() {
    return runEnd;
  }

  public void setRunEnd(Timestamp runEnd) {
    this.runEnd = runEnd;
  }

  public Timestamp getRunStart() {
    return runStart;
  }

  public void setRunStart(Timestamp runStart) {
    this.runStart = runStart;
  }

  public SiirtotiedostoProsessi createNewProcessBasedOnThis() {
    return new SiirtotiedostoProsessi(
        UUID.randomUUID().toString(),
        this.windowEnd,
        new Timestamp(System.currentTimeMillis()),
        new Timestamp(System.currentTimeMillis()),
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
