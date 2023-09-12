package fi.vm.sade.valintalaskenta.laskenta.resource;

import fi.vm.sade.valintalaskenta.domain.valintakoe.Hakutoive;
import fi.vm.sade.valintalaskenta.domain.valintakoe.Valintakoe;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeValinnanvaihe;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;

public class ValintakoelaskennanKumulatiivisetTulokset {
  private static final Logger LOG =
      LoggerFactory.getLogger(ValintakoelaskennanKumulatiivisetTulokset.class);

  private final ConcurrentHashMap<String, ValintakoeOsallistuminen> osallistumisetHakemuksittain =
      new ConcurrentHashMap<>();

  public ValintakoeOsallistuminen add(ValintakoeOsallistuminen osallistuminenLaskennasta) {
    return osallistumisetHakemuksittain.compute(
        osallistuminenLaskennasta.getHakemusOid(),
        (key, aiemminLaskettuOsallistuminen) -> {
          if (aiemminLaskettuOsallistuminen == null) {
            return osallistuminenLaskennasta;
          }
          if (LOG.isDebugEnabled()) {
            LOG.debug(
                String.format(
                    "Yhdistetään hakemuksen %s valintakoeosallistumiset %s ja %s",
                    osallistuminenLaskennasta.getHakemusOid(),
                    getKokeetAsStrings(aiemminLaskettuOsallistuminen),
                    getKokeetAsStrings(osallistuminenLaskennasta)));
          }
          return yhdista(osallistuminenLaskennasta, aiemminLaskettuOsallistuminen);
        });
  }

  public ValintakoeOsallistuminen get(String hakemusoid) {
    return osallistumisetHakemuksittain.get(hakemusoid);
  }

  private List<String> getKokeetAsStrings(ValintakoeOsallistuminen osallistuminen) {
    if (osallistuminen.getHakutoiveet() == null) {
      return Collections.emptyList();
    }
    return osallistuminen.getHakutoiveet().stream()
        .flatMap(ht -> ht.getValintakoeValinnanvaiheet().stream())
        .flatMap(vv -> vv.getValintakokeet().stream())
        .map(ToStringBuilder::reflectionToString)
        .collect(Collectors.toList());
  }

  private ValintakoeOsallistuminen yhdista(
      ValintakoeOsallistuminen laskettuOsallistuminen,
      ValintakoeOsallistuminen tallennettavaOsallistuminen) {
    BeanUtils.copyProperties(laskettuOsallistuminen, tallennettavaOsallistuminen, "hakutoiveet");

    laskettuOsallistuminen
        .getHakutoiveet()
        .forEach(
            laskettuHakutoive -> lisaaHakutoive(tallennettavaOsallistuminen, laskettuHakutoive));
    return tallennettavaOsallistuminen;
  }

  private void lisaaHakutoive(
      ValintakoeOsallistuminen osallistuminen, Hakutoive laskettuHakutoive) {
    Optional<Hakutoive> existingHakutoive =
        osallistuminen.getHakutoiveet().stream()
            .filter(ht -> ht.getHakukohdeOid().equals(laskettuHakutoive.getHakukohdeOid()))
            .findFirst();
    if (existingHakutoive.isPresent()) {
      laskettuHakutoive
          .getValintakoeValinnanvaiheet()
          .forEach(
              laskettuValinnanvaihe ->
                  lisaaValinnanvaihe(existingHakutoive.get(), laskettuValinnanvaihe));
    } else {
      osallistuminen.getHakutoiveet().add(laskettuHakutoive);
    }
  }

  private void lisaaValinnanvaihe(
      Hakutoive existingHakutoive, ValintakoeValinnanvaihe laskettuValinnanvaihe) {
    Optional<ValintakoeValinnanvaihe> existingValinnanvaihe =
        existingHakutoive.getValintakoeValinnanvaiheet().stream()
            .filter(
                vv -> vv.getValinnanVaihe().getValinnanvaiheOid().equals(laskettuValinnanvaihe.getValinnanVaihe().getValinnanvaiheOid()))
            .findFirst();
    if (existingValinnanvaihe.isPresent()) {
      laskettuValinnanvaihe
          .getValintakokeet()
          .forEach(laskettuValintakoe -> lisaaKoe(existingValinnanvaihe.get(), laskettuValintakoe));
    } else {
      existingHakutoive.getValintakoeValinnanvaiheet().add(laskettuValinnanvaihe);
    }
  }

  private void lisaaKoe(
      ValintakoeValinnanvaihe existingValinnanvaihe, Valintakoe laskettuValintakoe) {
    Optional<Valintakoe> existingValintakoe =
        existingValinnanvaihe.getValintakokeet().stream()
            .filter(koe -> koe.getValintakoeOid().equals(laskettuValintakoe.getValintakoeOid()))
            .findFirst();
    if (existingValintakoe.isPresent()) {
      if (LOG.isDebugEnabled()) {
        LOG.debug(
            String.format(
                "Kirjoitetaan valintakokeen %s yli tiedoilla %s",
                ToStringBuilder.reflectionToString(existingValintakoe.get()),
                ToStringBuilder.reflectionToString(laskettuValintakoe)));
      }
      BeanUtils.copyProperties(laskettuValintakoe, existingValintakoe.get());
    } else {
      existingValinnanvaihe.getValintakokeet().add(laskettuValintakoe);
    }
  }
}
