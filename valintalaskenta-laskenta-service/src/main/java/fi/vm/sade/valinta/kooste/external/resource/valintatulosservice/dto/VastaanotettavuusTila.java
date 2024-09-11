package fi.vm.sade.valinta.kooste.external.resource.valintatulosservice.dto;

import static java.util.Arrays.asList;

public enum VastaanotettavuusTila {
  EI_VASTAANOTETTAVISSA,
  VASTAANOTETTAVISSA_SITOVASTI,
  VASTAANOTETTAVISSA_EHDOLLISESTI;

  public boolean isVastaanottanut() {
    return asList(VASTAANOTETTAVISSA_SITOVASTI, VASTAANOTETTAVISSA_EHDOLLISESTI).contains(this);
  }
}
