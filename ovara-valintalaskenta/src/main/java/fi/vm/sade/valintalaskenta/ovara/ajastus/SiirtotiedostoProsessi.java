package fi.vm.sade.valintalaskenta.ovara.ajastus;

import java.sql.Timestamp;
import java.util.UUID;
import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.PersistenceCreator;
import org.springframework.data.relational.core.mapping.Column;
import org.springframework.data.relational.core.mapping.Table;

@Table("siirtotiedosto")
public class SiirtotiedostoProsessi {
  @Id
  @Column("execution_uuid")
  private UUID executionUuid;

  @Column("window_start")
  private Timestamp windowStart;

  @Column("window_end")
  private Timestamp windowEnd;

  @Column("run_start")
  private Timestamp runStart;

  @Column("run_end")
  private Timestamp runEnd;

  @Column("info")
  private SiirtotiedostoInfo info;

  @Column("success")
  private Boolean success;

  @Column("error_message")
  private String errorMessage;

  @PersistenceCreator
  public SiirtotiedostoProsessi(
      UUID executionUuid,
      Timestamp windowStart,
      Timestamp windowEnd,
      Timestamp runStart,
      Timestamp runEnd,
      SiirtotiedostoInfo info,
      Boolean success,
      String errorMessage) {
    this.executionUuid = executionUuid;
    this.windowStart = windowStart;
    this.windowEnd = windowEnd;
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

  public SiirtotiedostoInfo getInfo() {
    return info;
  }

  public void setInfo(SiirtotiedostoInfo info) {
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

  public UUID getExecutionUuid() {
    return executionUuid;
  }

  public void setExecutionUuid(UUID executionUuid) {
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
    SiirtotiedostoProsessi uusi =
        new SiirtotiedostoProsessi(
            null,
            this.windowEnd,
            new Timestamp(System.currentTimeMillis()),
            new Timestamp(System.currentTimeMillis()),
            null,
            null,
            null,
            "");
    return uusi;
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
