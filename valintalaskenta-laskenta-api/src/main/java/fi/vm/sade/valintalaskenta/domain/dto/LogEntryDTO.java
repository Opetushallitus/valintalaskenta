package fi.vm.sade.valintalaskenta.domain.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.Date;

@Schema(name = "LogEntryDTO", description = "Lokiviesti")
public class LogEntryDTO {

  @Schema(title = "Luomisajankohta", required = true)
  private Date luotu;

  @Schema(title = "Muokkaaja", required = true)
  private String muokkaaja;

  @Schema(title = "Muutos", required = true)
  private String muutos;

  @Schema(title = "Selite", required = true)
  private String selite;

  public Date getLuotu() {
    return luotu;
  }

  public void setLuotu(Date luotu) {
    this.luotu = luotu;
  }

  public String getMuokkaaja() {
    return muokkaaja;
  }

  public void setMuokkaaja(String muokkaaja) {
    this.muokkaaja = muokkaaja;
  }

  public String getMuutos() {
    return muutos;
  }

  public void setMuutos(String muutos) {
    this.muutos = muutos;
  }

  public String getSelite() {
    return selite;
  }

  public void setSelite(String selite) {
    this.selite = selite;
  }
}
