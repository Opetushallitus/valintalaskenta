package fi.vm.sade.valinta.kooste.external.resource.valintatulosservice.dto;

import static java.util.Arrays.asList;

public enum Valintatila {
  HYVAKSYTTY,
  HARKINNANVARAISESTI_HYVAKSYTTY,
  VARASIJALTA_HYVAKSYTTY,
  VARALLA,
  PERUUTETTU,
  PERUNUT,
  HYLATTY,
  PERUUNTUNUT,
  KESKEN;

  public boolean isHyvaksytty() {
    return asList(HYVAKSYTTY, HARKINNANVARAISESTI_HYVAKSYTTY, VARASIJALTA_HYVAKSYTTY)
        .contains(this);
  }
}
