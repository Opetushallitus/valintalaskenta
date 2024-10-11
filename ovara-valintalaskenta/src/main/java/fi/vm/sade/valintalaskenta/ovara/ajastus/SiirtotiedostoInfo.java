package fi.vm.sade.valintalaskenta.ovara.ajastus;

import com.fasterxml.jackson.annotation.JsonInclude;

@JsonInclude(JsonInclude.Include.NON_NULL)
public class SiirtotiedostoInfo {

  public int tuloksia = 0;
  public int osallistumisia = 0;

  public SiirtotiedostoInfo() {}

  public SiirtotiedostoInfo(int tuloksia, int osallistumisia) {
    this.tuloksia = tuloksia;
    this.osallistumisia = osallistumisia;
  }
}
