package fi.vm.sade.valintalaskenta.runner.resource.external.tarjonta.dto;

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
