package fi.vm.sade.valintalaskenta.domain.dto;

import fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila;
import io.swagger.v3.oas.annotations.media.Schema;
import java.math.BigDecimal;

@Schema(name = "MuokattuJonosijaArvoDTO", description = "Muokattu jonosija")
public class MuokattuJonosijaArvoDTO {

  @Schema(title = "Tila", required = true)
  private JarjestyskriteerituloksenTila tila;

  @Schema(title = "Arvo", required = true)
  private BigDecimal arvo;

  @Schema(title = "Selite", required = true)
  private String selite;

  public JarjestyskriteerituloksenTila getTila() {
    return tila;
  }

  public void setTila(JarjestyskriteerituloksenTila tila) {
    this.tila = tila;
  }

  public BigDecimal getArvo() {
    return arvo;
  }

  public void setArvo(BigDecimal arvo) {
    this.arvo = arvo;
  }

  public String getSelite() {
    return selite;
  }

  public void setSelite(String selite) {
    this.selite = selite;
  }
}
