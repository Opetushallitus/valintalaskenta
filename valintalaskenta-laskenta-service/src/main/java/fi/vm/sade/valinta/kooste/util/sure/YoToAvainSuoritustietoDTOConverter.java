package fi.vm.sade.valinta.kooste.util.sure;

import static java.util.Optional.ofNullable;
import static java.util.function.Function.identity;

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Maps;
import com.google.common.collect.Sets;
import fi.vm.sade.valinta.kooste.external.resource.suoritusrekisteri.dto.*;
import fi.vm.sade.valintalaskenta.domain.dto.AvainMetatiedotDTO;
import java.util.*;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.apache.commons.lang.StringUtils;
import org.joda.time.DateTime;
import org.joda.time.LocalDate;
import org.joda.time.LocalTime;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class YoToAvainSuoritustietoDTOConverter {
  private static final Logger LOG =
      LoggerFactory.getLogger(YoToAvainSuoritustietoDTOConverter.class);
  private static final String OSAKOE_ASTEIKKO = "OSAKOE";
  private static final Set<String> OSAKOETUNNUS_WHITELIST =
      Collections.unmodifiableSet(
          Sets.newHashSet(
              Arrays.asList("01", "02", "03", "04", "05", "06", "07", "08", "09", "10")));

  private static void lisaaSuorituspvm(Arvosana a, Map<String, String> x) {
    /*
    PK_SUORITUSLUKUKAUSI = 1/2
    1.1. - 31.7. ->  2
    1.8. -> 31.12. -> 1
            */
    // SUORITUSVUOSI: 2012,
    // SUORITUSLUKUKAUSI: 1,
    if (!StringUtils.isBlank(a.getMyonnetty())) {
      DateTime dt = new ArvosanaWrapper(a).getMyonnettyAsDateTime();
      x.put("SUORITUSVUOSI", "" + dt.getYear());
      LocalDate ld = new LocalDate(dt.getYear(), 8, 1);
      DateTime dt0 = ld.toDateTime(LocalTime.MIDNIGHT);
      if (!(dt.isEqual(dt0) || dt.isAfter(dt0))) {
        x.put("SUORITUSLUKUKAUSI", "1");
      } else {
        x.put("SUORITUSLUKUKAUSI", "2");
      }
    }
  }

  private static final Function<Arvosana, Map<String, String>> mapArvosana =
      a -> {
        Map<String, String> x = Maps.newHashMap();
        ofNullable(a.getArvio().getPisteet())
            .ifPresent(
                pisteet -> {
                  x.put("PISTEET", "" + pisteet);
                });
        x.put("ARVO", "" + a.getArvio().getArvosana());
        a.getAineyhdistelmarooli()
            .ifPresent(
                rooli -> {
                  x.put("ROOLI", "" + rooli);
                });
        lisaaSuorituspvm(a, x);
        return x;
      };
  private static final Function<Arvosana, Map<String, String>> mapArvosanaWithLisatieto =
      a -> {
        Map<String, String> x = Maps.newHashMap();
        x.put("LISATIETO", "" + a.getLisatieto());
        ofNullable(a.getArvio().getPisteet())
            .ifPresent(
                pisteet -> {
                  x.put("PISTEET", "" + pisteet);
                });
        x.put("ARVO", "" + a.getArvio().getArvosana());
        lisaaSuorituspvm(a, x);
        a.getAineyhdistelmarooli()
            .ifPresent(
                rooli -> {
                  x.put("ROOLI", "" + rooli);
                });
        return x;
      };

  public static List<AvainMetatiedotDTO> convert(Oppija oppija) {
    if (oppija == null || oppija.getSuoritukset() == null) {
      return Collections.emptyList();
    }
    List<SuoritusJaArvosanat> yoSuoritukset =
        oppija.getSuoritukset().stream()
            .filter(
                s ->
                    new SuoritusJaArvosanatWrapper(s).isYoTutkinto()
                        && !new SuoritusJaArvosanatWrapper(s).isItseIlmoitettu())
            .collect(Collectors.toList());
    if (yoSuoritukset.isEmpty()) {
      return Collections.emptyList();
    }
    List<Arvosana> yoArvosanat =
        yoSuoritukset.iterator().next().getArvosanat().stream()
            .flatMap(a -> normalisoi(a))
            .collect(Collectors.toList());
    Map<String, List<Arvosana>> arvosanaGrouping =
        yoArvosanat.stream()
            .collect(
                Collectors.groupingBy(
                    a -> ((Arvosana) a).getAine(),
                    Collectors.mapping(identity(), Collectors.toList())));
    return Stream.of(
            // arvosanat
            arvosanaGrouping.entrySet().stream()
                .map(
                    a ->
                        new AvainMetatiedotDTO(
                            a.getKey(),
                            a.getValue().stream().map(mapArvosana).collect(Collectors.toList()))),
            // ainereaali
            arvosanaGroup(
                "AINEREAALI",
                yoArvosanat.stream()
                    .filter(
                        find(
                            "UE", "UO", "ET", "FF", "PS", "HI", "FY", "KE", "BI", "GE", "TE",
                            "YH"))),
            arvosanaGroup("REAALI", yoArvosanat.stream().filter(find("RR", "RO", "RY"))),
            arvosanaGroup(
                "PITKA_KIELI",
                yoArvosanat.stream()
                    .filter(find("EA", "FA", "GA", "HA", "PA", "SA", "TA", "VA", "S9"))),
            arvosanaGroup(
                "KESKIPITKA_KIELI",
                yoArvosanat.stream().filter(find("EB", "FB", "GB", "HB", "PB", "SB", "TB", "VB"))),
            arvosanaGroup(
                "LYHYT_KIELI",
                yoArvosanat.stream()
                    .filter(find("EC", "FC", "GC", "L1", "PC", "SC", "TC", "VC", "KC", "L7"))),
            arvosanaGroup(
                "AIDINKIELI",
                yoArvosanat.stream().filter(find("O", "A", "I", "W", "Z", "O5", "A5"))))
        .flatMap(identity())
        .collect(Collectors.toList());
  }

  private static Predicate<Arvosana> find(String... arvosana) {
    final Set<String> aa = Sets.newHashSet(arvosana);
    return a -> aa.contains(a.getAine());
  }

  private static Stream<AvainMetatiedotDTO> arvosanaGroup(
      String group, Stream<Arvosana> arvosanat) {
    List<Arvosana> ar = arvosanat.collect(Collectors.toList());
    if (ar.isEmpty()) {
      return Stream.empty();
    } else {
      return Stream.of(
          new AvainMetatiedotDTO(
              group, ar.stream().map(mapArvosanaWithLisatieto).collect(Collectors.toList())));
    }
  }

  private static Stream<Arvosana> normalisoi(Arvosana a) {
    if (OSAKOE_ASTEIKKO.equals(a.getArvio().getAsteikko())) {
      String osakoe = a.getAine().split("_")[1];
      if (!OSAKOETUNNUS_WHITELIST.contains(osakoe)) {
        return Stream.empty(); // ei olla kiinnostuttu koodista
      }
      return Stream.of(
          new Arvosana(
              a.getId(),
              a.getSuoritus(),
              osakoe,
              a.getValinnainen(),
              a.getMyonnetty(),
              a.getSource(),
              oldExamRole(a),
              a.getArvio(),
              a.getLisatieto()));
    } else {
      return Stream.of(
          new Arvosana(
              a.getId(),
              a.getSuoritus(),
              aineMapper(a),
              a.getValinnainen(),
              a.getMyonnetty(),
              a.getSource(),
              oldExamRole(a),
              a.getArvio(),
              a.getLisatieto()));
    }
  }

  private static String aineMapper(Arvosana a) {
    return a.getKoetunnus()
        .map(YoToAvainSuoritustietoDTOConverter::koetunnusMapper)
        .orElseGet(
            () -> {
              String aine = aineMapper(a.getAine(), a.getLisatieto());
              LOG.warn(
                  "No koetunnus in YO arvosana: mapped aine '"
                      + a.getAine()
                      + "' and lisatieto '"
                      + a.getLisatieto()
                      + "' to aine "
                      + aine);
              return aine;
            });
  }

  private static ImmutableMap<String, ImmutableMap<String, String>> NEW_EXAM_ROLE_TO_OLD_EXAM_ROLE =
      ImmutableMap.of(
          "mother-tongue",
              ImmutableMap.<String, String>builder()
                  .put("A", "11")
                  .put("Z", "11")
                  .put("O", "11")
                  .put("W", "11")
                  .put("I", "11")
                  .put("J", "13")
                  .put("A5", "14")
                  .put("O5", "14")
                  .build(),
          "mandatory-subject",
              ImmutableMap.<String, String>builder()
                  .put("CA", "21")
                  .put("S2", "21")
                  .put("T1", "21")
                  .put("E2", "21")
                  .put("P1", "21")
                  .put("F1", "21")
                  .put("CB", "21")
                  .put("V2", "21")
                  .put("S1", "21")
                  .put("P2", "21")
                  .put("V1", "21")
                  .put("G2", "21")
                  .put("F2", "21")
                  .put("T2", "21")
                  .put("E1", "21")
                  .put("H2", "21")
                  .put("BB", "21")
                  // .put("RR","21")
                  // .put("VC","21")
                  // .put("PA","21")
                  // .put("M","21")
                  // .put("FA","21")
                  // .put("N","21")
                  // .put("SC","21")
                  // .put("SA","21")
                  // .put("BA","21")
                  // .put("PC","21")
                  // .put("EA","21")
                  // .put("FC","21")
                  // .put("TC","21")
                  // .put("VA","21")
                  // .put("EC","21")
                  /* END OF 21 */

                  .put("O", "22")
                  .put("W", "22")
                  .put("Z", "22")
                  .put("A", "22")
                  /* END OF 22 */
                  .put("PC", "31")
                  .put("DC", "31")
                  .put("TC", "31")
                  .put("SC", "31")
                  .put("SA", "31")
                  .put("BA", "31")
                  // .put("BB","31")
                  .put("FC", "31")
                  .put("EA", "31")
                  .put("PA", "31")
                  .put("L1", "31")
                  .put("VC", "31")
                  .put("CC", "31")
                  .put("EB", "31")
                  .put("EC", "31")
                  .put("S9", "31")
                  .put("L7", "31")
                  .put("VA", "31")
                  .put("FA", "31")
                  /* END OF 31 */

                  .put("UE", "41")
                  .put("RY", "41")
                  .put("HI", "41")
                  .put("GE", "41")
                  .put("PS", "41")
                  .put("YH", "41")
                  .put("FY", "41")
                  .put("RO", "41")
                  .put("TE", "41")
                  .put("BI", "41")
                  .put("KE", "41")
                  .put("FF", "41")
                  .put("UO", "41")
                  .put("ET", "41")
                  .put("RR", "41")
                  .put("M", "41")
                  .put("N", "41")
                  .build(),
          "optional-subject",
              ImmutableMap.<String, String>builder()
                  .put("A", "60")
                  .put("O", "60")
                  .put("Z", "60")
                  /* END OF 60 */
                  .put("L7", "61")
                  .put("VA", "61")
                  .put("IC", "61")
                  .put("L1", "61")
                  .put("VC", "61")
                  .put("EB", "61")
                  .put("PC", "61")
                  .put("DC", "61")
                  .put("SA", "61")
                  .put("S9", "61")
                  .put("EC", "61")
                  .put("FA", "61")
                  .put("KC", "61")
                  .put("TC", "61")
                  .put("GC", "61")
                  .put("QC", "61")
                  .put("P2", "61")
                  .put("EA", "61")
                  .put("FC", "61")
                  .put("PA", "61")
                  .put("SC", "61")
                  /* END OF 61 */
                  .put("CA", "62")
                  .put("BB", "62")
                  .put("A5", "62")
                  .put("BA", "62")
                  .put("CB", "62")
                  /* END OF 71 */
                  .put("RR", "71")
                  .put("PS", "71")
                  .put("UE", "71")
                  .put("GE", "71")
                  .put("RY", "71")
                  .put("KE", "71")
                  .put("FF", "71")
                  .put("YH", "71")
                  .put("HI", "71")
                  .put("ET", "71")
                  .put("TE", "71")
                  .put("RO", "71")
                  .put("FY", "71")
                  .put("UO", "71")
                  .put("BI", "71")
                  /* END OF 71 */
                  .put("M", "81")
                  .put("N", "81")
                  /* END OF 81 */
                  .build());

  private static Optional<Map<String, String>> oldExamRole(String examId, String newExamRole) {
    return Optional.ofNullable(
            NEW_EXAM_ROLE_TO_OLD_EXAM_ROLE.getOrDefault(newExamRole, ImmutableMap.of()).get(examId))
        .map(
            oldExamRole ->
                ImmutableMap.of(
                    "aineyhdistelmarooli", oldExamRole,
                    "koetunnus", examId));
  }

  private static Map<String, String> oldExamRole(Arvosana a) {
    Optional<String> examId = a.getKoetunnus();
    Optional<String> examRole = a.getAineyhdistelmarooli();
    if (examId.isPresent() && examRole.isPresent()) {
      return oldExamRole(examId.get(), examRole.get()).orElse(a.getLahdeArvot());
    }
    return a.getLahdeArvot();
  }

  private static String koetunnusMapper(String koetunnus) {
    switch (koetunnus) {
      case "E1":
        return "EA";
      case "E2":
        return "EB";
      case "F1":
        return "FA";
      case "F2":
        return "FB";
      case "G1":
        return "GA";
      case "G2":
        return "GB";
      case "H1":
        return "HA";
      case "H2":
        return "HB";
      case "P1":
        return "PA";
      case "P2":
        return "PB";
      case "S1":
        return "SA";
      case "S2":
        return "SB";
      case "T1":
        return "TA";
      case "T2":
        return "TB";
      case "V1":
        return "VA";
      case "V2":
        return "VB";
      default:
        return koetunnus;
    }
  }

  private static String aineMapper(String aine, String lisatieto) {
    if (aine.equals("AINEREAALI")) {
      return lisatieto;
    } else if (aine.equals("REAALI")) { // Vanha reaali
      switch (lisatieto) {
        case "ET":
          return "RY";
        case "UO":
          return "RO";
        case "UE":
          return "RR";
        default:
          return "REAALI_" + lisatieto;
      }

    } else if (aine.equals("A")) { // Pitkät kielit
      switch (lisatieto) {
        case "SA":
          return "SA";
        case "EN":
          return "EA";
        case "UN":
          return "HA";
        case "VE":
          return "VA";
        case "IT":
          return "TA";
        case "RA":
          return "FA";
        case "FI":
          return "CA";
        case "RU":
          return "BA";
        case "ES":
          return "PA";
        case "PG":
          return "GA";
        default:
          return "A_" + lisatieto;
      }
    } else if (aine.equals("B")) { // Keskipitkät kielet
      switch (lisatieto) {
        case "SA":
          return "SB";
        case "EN":
          return "EB";
        case "UN":
          return "HB";
        case "VE":
          return "VB";
        case "IT":
          return "TB";
        case "RA":
          return "FB";
        case "FI":
          return "CB";
        case "RU":
          return "BB";
        case "ES":
          return "PB";
        case "PG":
          return "GB";
        default:
          return "B_" + lisatieto;
      }
    } else if (aine.equals("C")) { // Lyhyet kielet
      switch (lisatieto) {
        case "SA":
          return "SC";
        case "EN":
          return "EC";
        case "UN":
          return "HC";
        case "VE":
          return "VC";
        case "IT":
          return "TC";
        case "RA":
          return "FC";
        case "FI":
          return "CC";
        case "RU":
          return "BC";
        case "ES":
          return "PC";
        case "PG":
          return "GC";
        case "IS":
          return "IC";
        case "ZA":
          return "DC";
        case "KR":
          return "KC";
        case "QS":
          return "QC";
        case "LA":
          return "L7";
        default:
          return "C_" + lisatieto;
      }
    } else if (aine.equals("AI")) { // Äidinkielet
      switch (lisatieto) {
        case "IS":
          return "I";
        case "RU":
          return "O";
        case "FI":
          return "A";
        case "ZA":
          return "Z";
        case "QS":
          return "W";
        default:
          return "AI_" + lisatieto;
      }
    } else if (aine.equals("VI2")) { // Suomi/Ruotsi toisena kielenä
      switch (lisatieto) {
        case "RU":
          return "O5";
        case "FI":
          return "A5";
        default:
          return "VI2_" + lisatieto;
      }
    } else if (aine.equals("PITKA")) { // Pitkä matematiikka
      switch (lisatieto) {
        case "MA":
          return "M";
        default:
          return "PITKA_" + lisatieto;
      }
    } else if (aine.equals("LYHYT")) { // Lyhyt matematiikka
      switch (lisatieto) {
        case "MA":
          return "N";
        default:
          return "LYHYT_" + lisatieto;
      }
    } else if (aine.equals("SAKSALKOUL")) { // Saksalaisen koulun saksan kielen koe
      switch (lisatieto) {
        case "SA":
          return "S9";
        default:
          return "SAKSALKOUL_" + lisatieto;
      }
    } else if (aine.equals("KYPSYYS")) { // Englanninkielinen kypsyyskoe
      switch (lisatieto) {
        case "EN":
          return "J";
        default:
          return "KYPSYYS_" + lisatieto;
      }
    } else if (aine.equals("D")) { // Latina
      switch (lisatieto) {
        case "LA":
          return "L1";
        default:
          return "D_" + lisatieto;
      }
    } else {
      return "XXX";
    }
  }
}
