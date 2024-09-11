package fi.vm.sade.valinta.kooste.external.resource.tarjonta;

import fi.vm.sade.tarjonta.service.resources.v1.dto.HakukohdeV1RDTO;
import java.util.*;
import java.util.stream.Collectors;

public class TarjontaHakukohde extends AbstractHakukohde {
  public final Set<TarjontaValintakoe> valintakokeet;

  public TarjontaHakukohde(
      String oid,
      Tila tila,
      Map<String, String> nimi,
      String hakuOid,
      Set<String> tarjoajaOids,
      Set<String> toteutusOids,
      String hakukohteetUri,
      Set<String> pohjakoulutusvaatimusUrit,
      Integer valintojenAloituspaikat,
      Set<TarjontaValintakoe> valintakokeet,
      Map<String, String> ohjeetUudelleOpiskelijalle) {
    super(
        oid,
        tila,
        nimi,
        hakuOid,
        tarjoajaOids,
        toteutusOids,
        hakukohteetUri,
        pohjakoulutusvaatimusUrit,
        valintojenAloituspaikat,
        ohjeetUudelleOpiskelijalle);
    this.valintakokeet = valintakokeet;
  }

  public TarjontaHakukohde(HakukohdeV1RDTO dto) {
    super(
        dto.getOid(),
        Tila.valueOf(dto.getTila().name()),
        dto.getHakukohteenNimet(),
        dto.getHakuOid(),
        dto.getTarjoajaOids(),
        new HashSet<>(dto.getHakukohdeKoulutusOids()),
        dto.getHakukohteenNimiUri(),
        dto.getPohjakoulutusvaatimus() != null
            ? Collections.singleton(dto.getPohjakoulutusvaatimus())
            : new HashSet<>(),
        dto.getValintojenAloituspaikatLkm(),
        new HashMap<>());

    this.valintakokeet =
        dto.getValintakokeet().stream().map(TarjontaValintakoe::new).collect(Collectors.toSet());
    if (dto.getOhjeetUudelleOpiskelijalle() != null) {
      this.ohjeetUudelleOpiskelijalle.put("kieli_fi", dto.getOhjeetUudelleOpiskelijalle());
      this.ohjeetUudelleOpiskelijalle.put("kieli_sv", dto.getOhjeetUudelleOpiskelijalle());
      this.ohjeetUudelleOpiskelijalle.put("kieli_en", dto.getOhjeetUudelleOpiskelijalle());
    }
  }
}
