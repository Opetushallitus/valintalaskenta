package fi.vm.sade.valintalaskenta.ovara.ajastus;

import com.fasterxml.jackson.annotation.JsonInclude;

@JsonInclude(JsonInclude.Include.NON_NULL)
public class SiirtotiedostoInfo {

  public int tuloksia = 0;
  public int osallistumisia = 0;
  public int pisteita = 0;

  public SiirtotiedostoInfo() {}

  public SiirtotiedostoInfo(int tuloksia, int osallistumisia, int pisteita) {
    this.tuloksia = tuloksia;
    this.osallistumisia = osallistumisia;
    this.pisteita = pisteita;
  }

  @Override
  public String toString() {
    return String.format("Tuloksia %s, osallistumisia %s", tuloksia, osallistumisia);
  }
}
