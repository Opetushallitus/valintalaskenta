package fi.vm.sade.valintalaskenta.domain.dto;

import fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila;
import io.swagger.v3.oas.annotations.media.Schema;
import java.math.BigDecimal;
import java.util.Map;

@Schema(name = "JarjestyskriteeritulosDTO", description = "Järjestyskriteerin tulos")
public class JarjestyskriteeritulosDTO implements Comparable<JarjestyskriteeritulosDTO> {
  public JarjestyskriteeritulosDTO() {}

  public JarjestyskriteeritulosDTO(
      final BigDecimal arvo,
      final JarjestyskriteerituloksenTila tila,
      final Map<String, String> kuvaus,
      final int prioriteetti,
      final String nimi) {
    this.arvo = arvo;
    this.tila = tila;
    this.kuvaus = kuvaus;
    this.prioriteetti = prioriteetti;
    this.nimi = nimi != null ? nimi.intern() : null;
  }

  @Schema(description = "Järjestyskriteerin lukuarvo", required = true)
  private BigDecimal arvo;

  @Schema(description = "Järjestyskriteerin tila", required = true)
  private JarjestyskriteerituloksenTila tila;

  @Schema(description = "Monikielinen kuvaus (esim. hylkäyksen syy)")
  private Map<String, String> kuvaus;

  @Schema(description = "Järjestyskriteerin prioriteetti", required = true)
  private int prioriteetti;

  @Schema(description = "Järjestyskriteerin nimi")
  private String nimi;

  @Override
  public int compareTo(JarjestyskriteeritulosDTO o) {
    return Integer.valueOf(prioriteetti).compareTo(o.getPrioriteetti());
  }

  public BigDecimal getArvo() {
    return arvo;
  }

  public void setArvo(BigDecimal arvo) {
    this.arvo = arvo;
  }

  public JarjestyskriteerituloksenTila getTila() {
    return tila;
  }

  public void setTila(JarjestyskriteerituloksenTila tila) {
    this.tila = tila;
  }

  public int getPrioriteetti() {
    return prioriteetti;
  }

  public void setPrioriteetti(int prioriteetti) {
    this.prioriteetti = prioriteetti;
  }

  public String getNimi() {
    return nimi;
  }

  public void setNimi(String nimi) {
    this.nimi = nimi != null ? nimi.intern() : null;
  }

  public Map<String, String> getKuvaus() {
    return kuvaus;
  }

  public void setKuvaus(Map<String, String> kuvaus) {
    this.kuvaus = kuvaus;
  }
}
