package fi.vm.sade.valintalaskenta.runner.resource.external.tarjonta.dto;

public class ResultSearch {
  private ResultTulos result;

  public ResultSearch(ResultTulos result) {
    this.result = result;
  }

  public ResultTulos getResult() {
    return result;
  }
}
