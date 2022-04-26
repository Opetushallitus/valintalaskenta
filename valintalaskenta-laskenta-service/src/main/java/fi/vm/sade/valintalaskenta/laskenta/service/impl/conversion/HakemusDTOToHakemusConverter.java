package fi.vm.sade.valintalaskenta.laskenta.service.impl.conversion;

import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakemus;
import fi.vm.sade.service.valintaperusteet.laskenta.api.Hakutoive;
import fi.vm.sade.valintalaskenta.domain.dto.AvainArvoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.AvainMetatiedotDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;
import fi.vm.sade.valintalaskenta.domain.dto.HakukohdeDTO;
import io.circe.Encoder;
import io.circe.Encoder$;
import io.circe.Json;
import io.circe.ParsingFailure;
import io.circe.jawn.JawnParser;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;
import scala.collection.immutable.List$;
import scala.util.Either;

;

@Component("HakemusDTOKonvertteri")
public class HakemusDTOToHakemusConverter implements Converter<HakemusDTO, Hakemus> {
  private static final Logger LOGGER = LoggerFactory.getLogger(HakemusDTOToHakemusConverter.class);
  private final JawnParser circeParser = new JawnParser();

  private static Function<HakukohdeDTO, Hakutoive> getHakutoive =
      hakukohdeDTO -> new Hakutoive(hakukohdeDTO.getOid(), hakukohdeDTO.getHakukohdeRyhmatOids());

  private static Function<HakukohdeDTO, Integer> getPrioriteetti = HakukohdeDTO::getPrioriteetti;

  public Hakemus convert(HakemusDTO dto) {
    try {
      Map<Integer, Hakutoive> prioriteettiHakukohde =
          dto.getHakukohteet().stream().collect(Collectors.toMap(getPrioriteetti, getHakutoive));
      Map<String, String> target =
          dto.getAvaimet().stream()
              .collect(
                  Collectors.toMap(
                      AvainArvoDTO::getAvain, AvainArvoDTO::getArvo, (s, a) -> s + ", " + a));
      Map<String, List<Map<String, String>>> metatiedot =
          dto.getAvainMetatiedotDTO().stream()
              .collect(
                  Collectors.toMap(
                      AvainMetatiedotDTO::getAvain,
                      AvainMetatiedotDTO::getMetatiedot,
                      (s, a) -> {
                        s.addAll(a);
                        return s;
                      }));
      return new Hakemus(
          dto.getHakemusoid(),
          prioriteettiHakukohde,
          target,
          metatiedot,
          stringToCirceJson(dto.getKoskiOpiskeluoikeudetJson()));
    } catch (Exception e) {
      LOGGER.error(
          "Converting HakemusDTO to Hakemus failed for hakemus: {}", dto.getHakemusoid(), e);
      dto.getAvaimet()
          .forEach(
              avain -> {
                LOGGER.error(
                    "Hakemus: {}, avain: {}, arvo: {}",
                    dto.getHakemusoid(), avain.getAvain(), avain.getArvo());
              });
      throw e;
    }
  }

  private Json stringToCirceJson(String koskiOpiskeluoikeudetJson) {
    if (StringUtils.isBlank(koskiOpiskeluoikeudetJson)) {
      return createEmptyCirceJsonArray();
    }
    Either<ParsingFailure, Json> parseResult = circeParser.parse(koskiOpiskeluoikeudetJson);
    if (parseResult.isRight()) {
      return parseResult.right().get();
    }
    throw new IllegalArgumentException(
        "Virhe jäsennettäessä JSON-dokumenttia Koskesta. JSON='" + koskiOpiskeluoikeudetJson + "'",
        parseResult.left().get());
  }

  private Json createEmptyCirceJsonArray() {
    scala.collection.immutable.List<Object> empty = List$.MODULE$.<Object>empty();
    return Encoder$.MODULE$.encodeList(Encoder.AsArray$.MODULE$.apply(null)).apply(empty);
  }
}
