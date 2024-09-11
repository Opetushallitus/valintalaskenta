package fi.vm.sade.valinta.kooste.external.resource.kouta;

import fi.vm.sade.valinta.kooste.external.resource.kouta.dto.KoutaHakukohdeDTO;
import fi.vm.sade.valinta.kooste.external.resource.tarjonta.AbstractHakukohde;
import java.math.BigDecimal;
import java.util.*;
import java.util.stream.Collectors;

public class KoutaHakukohde extends AbstractHakukohde {
  public final Set<KoutaValintakoe> valintakokeet;
  public final Set<KoutaValintakoe> valintaperusteValintakokeet;
  public final BigDecimal alinHyvaksyttyKeskiarvo;
  public final List<PainotettuArvosana> painotetutArvosanat;
  public final String hakukohdeKoodiUri;
  public final String koulutustyyppikoodi;

  public KoutaHakukohde(
      String oid,
      Tila tila,
      Map<String, String> nimi,
      String hakuOid,
      Set<String> tarjoajaOids,
      Set<String> toteutusOids,
      Set<String> pohjakoulutusvaatimusUrit,
      Integer valintojenAloituspaikat,
      Map<String, String> ohjeetUudelleOpiskelijalle,
      Set<KoutaValintakoe> valintakokeet,
      Set<KoutaValintakoe> valintaperusteValintakokeet,
      BigDecimal alinHyvaksyttyKeskiarvo,
      List<PainotettuArvosana> painotetutArvosanat,
      String hakukohdeKoodiUri,
      String koulutustyyppikoodi) {
    super(
        oid,
        tila,
        nimi,
        hakuOid,
        tarjoajaOids,
        toteutusOids,
        null,
        pohjakoulutusvaatimusUrit,
        valintojenAloituspaikat,
        ohjeetUudelleOpiskelijalle);
    this.valintakokeet = valintakokeet;
    this.valintaperusteValintakokeet = valintaperusteValintakokeet;
    this.alinHyvaksyttyKeskiarvo = alinHyvaksyttyKeskiarvo;
    this.painotetutArvosanat = painotetutArvosanat;
    this.hakukohdeKoodiUri = hakukohdeKoodiUri;
    this.koulutustyyppikoodi = koulutustyyppikoodi;
  }

  public KoutaHakukohde(KoutaHakukohdeDTO dto) {
    super(
        dto.oid,
        parseTila(dto.tila),
        new HashMap<>(),
        dto.hakuOid,
        Set.of(dto.tarjoaja),
        Collections.singleton(dto.toteutusOid),
        null,
        dto.pohjakoulutusvaatimusKoodiUrit,
        dto.aloituspaikat,
        new HashMap<>());

    this.valintakokeet =
        dto.valintakokeet.stream().map(KoutaValintakoe::new).collect(Collectors.toSet());
    dto.nimi.forEach((kieli, nimi) -> this.nimi.put("kieli_" + kieli, nimi));
    if (dto.uudenOpiskelijanUrl != null) {
      dto.uudenOpiskelijanUrl.forEach(
          (kieli, uudenOpiskelijanUrl) ->
              this.ohjeetUudelleOpiskelijalle.put("kieli_" + kieli, uudenOpiskelijanUrl));
    }
    this.alinHyvaksyttyKeskiarvo = dto.alinHyvaksyttyKeskiarvo;
    this.valintaperusteValintakokeet =
        dto.valintaperusteValintakokeet.stream()
            .map(KoutaValintakoe::new)
            .collect(Collectors.toSet());
    this.painotetutArvosanat =
        dto.painotetutArvosanat.stream().map(PainotettuArvosana::new).collect(Collectors.toList());
    this.hakukohdeKoodiUri = dto.hakukohde != null ? dto.hakukohde.koodiUri : null;
    this.koulutustyyppikoodi = dto.koulutustyyppikoodi;
  }

  public Optional<KoutaValintakoe> getValintakoeOfType(String valintakoeTypeUri) {
    return getValintakoeOfType(this.valintakokeet, valintakoeTypeUri)
        .or(() -> getValintakoeOfType(this.valintaperusteValintakokeet, valintakoeTypeUri));
  }

  private static Optional<KoutaValintakoe> getValintakoeOfType(
      Collection<KoutaValintakoe> valintakokeet, String valintakoeTypeUri) {
    return valintakokeet.stream()
        .filter(vk -> vk.valintakokeentyyppiUri.split("#")[0].equals(valintakoeTypeUri))
        .findFirst();
  }

  private static Tila parseTila(String tila) {
    switch (tila) {
      case "julkaistu":
      case "arkistoitu":
        return Tila.JULKAISTU;
      case "tallennettu":
        return Tila.LUONNOS;
      default:
        return Tila.POISTETTU;
    }
  }
}
