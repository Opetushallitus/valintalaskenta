package fi.vm.sade.valintalaskenta.domain.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import java.util.Date;

@ApiModel(value = "LogEntryDTO", description = "Lokiviesti")
public class LogEntryDTO {

  @ApiModelProperty(value = "Luomisajankohta", required = true)
  private Date luotu;

  @ApiModelProperty(value = "Muokkaaja", required = true)
  private String muokkaaja;

  @ApiModelProperty(value = "Muutos", required = true)
  private String muutos;

  @ApiModelProperty(value = "Selite", required = true)
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
