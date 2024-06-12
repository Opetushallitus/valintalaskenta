package fi.vm.sade.valintalaskenta.domain.dto.siirtotiedosto;

import fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila;
import java.math.BigDecimal;
import java.util.Map;

public class JarjestyskriteeritulosSiirtotiedostoDTO
    implements Comparable<JarjestyskriteeritulosSiirtotiedostoDTO> {
  private BigDecimal arvo;

  private JarjestyskriteerituloksenTila tila;

  private Map<String, String> kuvaus;

  private int prioriteetti;

  private String nimi;

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

  public Map<String, String> getKuvaus() {
    return kuvaus;
  }

  public void setKuvaus(Map<String, String> kuvaus) {
    this.kuvaus = kuvaus;
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

  @Override
  public int compareTo(JarjestyskriteeritulosSiirtotiedostoDTO o) {
    return Integer.valueOf(prioriteetti).compareTo(o.getPrioriteetti());
  }
}
