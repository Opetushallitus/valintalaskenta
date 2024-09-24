package fi.vm.sade.valinta.kooste.external.resource.tarjonta.dto;

import java.util.List;

public class HakukohderyhmaHakukohde {
  public String oid;
  public List<String> hakukohderyhmat;

  public HakukohderyhmaHakukohde(String hakukohdeOid, List<String> hakukohderyhmat) {
    this.oid = hakukohdeOid;
    this.hakukohderyhmat = hakukohderyhmat;
  }

  public String getHakukohdeOid() {
    return oid;
  }

  public List<String> getHakukohderyhmat() {
    return hakukohderyhmat;
  }
}
