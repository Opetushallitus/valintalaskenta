package fi.vm.sade.valinta.kooste.external.resource.kouta.dto;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class KoutaHakukohdeDTO {
  public final String oid;
  public final String tila;
  public final Map<String, String> nimi;
  public final String hakuOid;
  public final String tarjoaja;
  public final String toteutusOid;
  public final Integer aloituspaikat;
  public final Set<KoutaValintakoeDTO> valintakokeet;
  public final Set<String> pohjakoulutusvaatimusKoodiUrit;
  public final Map<String, String> uudenOpiskelijanUrl;
  public final BigDecimal alinHyvaksyttyKeskiarvo;
  public final List<PainotettuArvosanaDTO> painotetutArvosanat;
  public final Set<KoutaValintakoeDTO> valintaperusteValintakokeet;
  public final KoodiUriDTO hakukohde;
  public final String koulutustyyppikoodi;

  private KoutaHakukohdeDTO() {
    this.oid = null;
    this.tila = null;
    this.nimi = null;
    this.hakuOid = null;
    this.tarjoaja = null;
    this.toteutusOid = null;
    this.aloituspaikat = null;
    this.valintakokeet = null;
    this.pohjakoulutusvaatimusKoodiUrit = null;
    this.uudenOpiskelijanUrl = null;
    this.alinHyvaksyttyKeskiarvo = null;
    this.painotetutArvosanat = null;
    this.valintaperusteValintakokeet = null;
    this.hakukohde = null;
    this.koulutustyyppikoodi = null;
  }

  public static class KoodiUriDTO {
    public final String koodiUri;

    private KoodiUriDTO(String koodiUri) {
      this.koodiUri = koodiUri;
    }
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;

    KoutaHakukohdeDTO that = (KoutaHakukohdeDTO) o;

    return oid.equals(that.oid);
  }

  @Override
  public int hashCode() {
    return oid.hashCode();
  }
}
