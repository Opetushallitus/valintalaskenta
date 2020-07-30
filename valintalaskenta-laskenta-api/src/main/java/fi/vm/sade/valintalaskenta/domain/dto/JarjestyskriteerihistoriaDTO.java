package fi.vm.sade.valintalaskenta.domain.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;

@ApiModel(value = "JarjestyskriteerihistoriaDTO", description = "Järjestyskriteerihistoria")
public class JarjestyskriteerihistoriaDTO {

  @ApiModelProperty(value = "Historia JSON", required = true)
  private String historia;

  public String getHistoria() {
    return historia;
  }

  public void setHistoria(String historia) {
    this.historia = historia;
  }
}
