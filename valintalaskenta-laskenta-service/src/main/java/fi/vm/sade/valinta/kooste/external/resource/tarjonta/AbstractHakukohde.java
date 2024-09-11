package fi.vm.sade.valinta.kooste.external.resource.tarjonta;

import java.util.Map;
import java.util.Set;

public abstract class AbstractHakukohde {
  public final String oid;
  public final Tila tila;
  public final Map<String, String> nimi;
  public final String hakuOid;
  public final Set<String> tarjoajaOids;
  public final Set<String> toteutusOids;
  public final String hakukohteetUri;
  public final Set<String> pohjakoulutusvaatimusUrit;
  public final Integer valintojenAloituspaikat;
  public final Map<String, String> ohjeetUudelleOpiskelijalle;

  protected AbstractHakukohde(
      String oid,
      Tila tila,
      Map<String, String> nimi,
      String hakuOid,
      Set<String> tarjoajaOids,
      Set<String> toteutusOids,
      String hakukohteetUri,
      Set<String> pohjakoulutusvaatimusUrit,
      Integer valintojenAloituspaikat,
      Map<String, String> ohjeetUudelleOpiskelijalle) {
    this.oid = oid;
    this.tila = tila;
    this.nimi = nimi;
    this.hakuOid = hakuOid;
    this.tarjoajaOids = tarjoajaOids;
    this.toteutusOids = toteutusOids;
    this.hakukohteetUri = hakukohteetUri;
    this.pohjakoulutusvaatimusUrit = pohjakoulutusvaatimusUrit;
    this.valintojenAloituspaikat = valintojenAloituspaikat;
    this.ohjeetUudelleOpiskelijalle = ohjeetUudelleOpiskelijalle;
  }

  public enum Tila {
    POISTETTU,
    LUONNOS,
    VALMIS,
    JULKAISTU,
    PERUTTU,
    KOPIOITU,
    PUUTTEELLINEN;
  }
}
