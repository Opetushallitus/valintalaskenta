package fi.vm.sade.valintalaskenta.domain.dto;

import io.swagger.v3.oas.annotations.media.Schema;

@Schema(name = "JarjestyskriteerihistoriaDTO", description = "Järjestyskriteerihistoria")
public class JarjestyskriteerihistoriaDTO {

  @Schema(name = "Historia JSON", required = true)
  private String historia;

  public String getHistoria() {
    return historia;
  }

  public void setHistoria(String historia) {
    this.historia = historia;
  }
}
