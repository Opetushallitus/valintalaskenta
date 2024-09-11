package fi.vm.sade.valinta.kooste.external.resource.tarjonta.dto;

import java.util.List;

public class KoutaKoulutus {
  public final String oid;
  public final List<String> koulutusKoodiUrit;

  private KoutaKoulutus() {
    this.oid = null;
    this.koulutusKoodiUrit = null;
  }
}
