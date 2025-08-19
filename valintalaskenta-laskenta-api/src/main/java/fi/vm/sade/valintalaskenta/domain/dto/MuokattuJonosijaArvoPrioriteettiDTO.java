package fi.vm.sade.valintalaskenta.domain.dto;

import fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila;
import io.swagger.v3.oas.annotations.media.Schema;
import java.math.BigDecimal;

@Schema(
    name = "MuokattuJonosijaArvoPrioriteettiDTO",
    description = "Muokattu jonosija prioriteetilla")
public class MuokattuJonosijaArvoPrioriteettiDTO {

  @Schema(description = "Tila")
  private JarjestyskriteerituloksenTila tila;

  @Schema(description = "Arvo")
  private BigDecimal arvo;

  @Schema(description = "Selite")
  private String selite;

  @Schema(description = "Jonosijan järjestyskriteerin prioriteetti")
  private Integer jarjestyskriteeriPrioriteetti;

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

  public Integer getJarjestyskriteriPrioriteetti() {
    return jarjestyskriteeriPrioriteetti;
  }

  public void setJarjestyskriteeriPrioriteetti(Integer jarjestyskriteeriPrioriteetti) {
    this.jarjestyskriteeriPrioriteetti = jarjestyskriteeriPrioriteetti;
  }
}
