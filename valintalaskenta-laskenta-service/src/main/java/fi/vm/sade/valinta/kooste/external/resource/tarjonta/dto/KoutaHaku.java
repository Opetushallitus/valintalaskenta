package fi.vm.sade.valinta.kooste.external.resource.tarjonta.dto;

import java.util.List;
import java.util.Map;

public class KoutaHaku {
  public final String oid;
  public final Map<String, String> nimi;
  public final String organisaatioOid;
  public final String kohdejoukkoKoodiUri;
  public final String hakulomakeAtaruId;
  public final List<KoutaHakuaika> hakuajat;
  public final String alkamiskausiKoodiUri;
  public final String alkamisvuosi;

  private KoutaHaku() {
    this.oid = null;
    this.nimi = null;
    this.organisaatioOid = null;
    this.kohdejoukkoKoodiUri = null;
    this.hakulomakeAtaruId = null;
    this.hakuajat = null;
    this.alkamiskausiKoodiUri = null;
    this.alkamisvuosi = null;
  }
}
