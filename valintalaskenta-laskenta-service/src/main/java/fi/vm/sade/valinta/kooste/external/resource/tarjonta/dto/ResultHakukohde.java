package fi.vm.sade.valinta.kooste.external.resource.tarjonta.dto;

import java.util.List;

public class ResultHakukohde {
  private String oid;
  private List<ResultRyhmaliitos> ryhmaliitokset;

  public ResultHakukohde(String oid) {
    this.oid = oid;
  }

  public String getOid() {
    return oid;
  }

  public List<ResultRyhmaliitos> getRyhmaliitokset() {
    return ryhmaliitokset;
  }

  public void setRyhmaliitokset(List<ResultRyhmaliitos> ryhmaliitokset) {
    this.ryhmaliitokset = ryhmaliitokset;
  }
}
