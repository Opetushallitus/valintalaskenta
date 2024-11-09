package fi.vm.sade.valinta.kooste.external.resource.tarjonta.dto;

import java.util.List;

public class ResultTulos {
  private List<ResultOrganization> tulokset;

  public ResultTulos(List<ResultOrganization> tulokset) {
    this.tulokset = tulokset;
  }

  public List<ResultOrganization> getTulokset() {
    return tulokset;
  }
}
