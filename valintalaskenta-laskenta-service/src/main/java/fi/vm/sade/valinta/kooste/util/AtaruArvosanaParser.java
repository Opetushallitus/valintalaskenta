package fi.vm.sade.valinta.kooste.util;

import fi.vm.sade.valintalaskenta.domain.dto.AvainArvoDTO;
import java.util.*;
import java.util.stream.Collectors;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class AtaruArvosanaParser {
  private static final Logger LOG = LoggerFactory.getLogger(AtaruArvosanaParser.class);

  // Toisen asteen ataruhakemuksilta löytyy tällä hetkellä vain peruskoulun arvosanoja
  private static final String prefix = "PK_";

  public static String convertAtaruAidinkieliValue(String valueFromAtaru) {
    switch (valueFromAtaru) {
      case "suomi-aidinkielena":
        return "FI";
      case "suomi-toisena-kielena":
        return "FI_2";
      case "suomi-viittomakielisille":
        return "FI_VK";
      case "suomi-saamenkielisille":
        return "FI_SE";
      case "ruotsi-aidinkielena":
        return "SV";
      case "ruotsi-toisena-kielena":
        return "SV_2";
      case "ruotsi-viittomakielisille":
        return "SV_VK";
      case "saame-aidinkielena":
        return "SE";
      case "romani-aidinkielena":
        return "RI";
      case "viittomakieli-aidinkielena":
        return "VK";
      case "muu-oppilaan-aidinkieli":
        return "XX";
    }
    return "XX";
  }

  // Käsittelee tämänmuotoista dataa:
  // "oppiaine-valinnainen-kieli_group1": "oppiaine-valinnainen-kieli-a2",
  // "oppiaine-valinnainen-kieli_group0": "oppiaine-valinnainen-kieli-b2",
  // "oppiaine-valinnainen-kieli_group2": "",
  // "oppimaara-kieli-valinnainen-kieli_group1": "EN",
  // "oppimaara-kieli-valinnainen-kieli_group0": "HE",
  // "oppimaara-kieli-valinnainen-kieli_group2": "",
  // "arvosana-valinnainen-kieli_group2": "",
  // "arvosana-valinnainen-kieli_group1": "arvosana-valinnainen-kieli-6",
  // "arvosana-valinnainen-kieli_group0": "arvosana-valinnainen-kieli-8"
  public static List<AvainArvoDTO> convertValinnaisetKielet(
      Map<String, AvainArvoDTO> keyValues, String hakemusOid) {

    List<AvainArvoDTO> r = new ArrayList<>();

    Map<String, List<AvainArvoDTO>> grouped =
        keyValues.values().stream()
            .filter(
                dto ->
                    dto.getAvain().contains("valinnainen-kieli") && !dto.getAvain().contains("-a-"))
            .collect(
                Collectors.groupingBy(
                    dto -> dto.getAvain().substring(dto.getAvain().length() - 1)));

    Set<String> langs = new HashSet<>(); // A1, A2, B2 jne.

    grouped.forEach(
        (key, value) -> {
          String lang =
              value.stream()
                  .filter(dto -> dto.getAvain().contains("oppiaine"))
                  .findFirst()
                  .map(dto -> StringUtils.substringAfterLast(dto.getArvo(), "-"))
                  .orElse("")
                  .toUpperCase();
          if (!lang.isEmpty()) {
            langs.add(lang);
          }
        });

    for (String aineKey : langs) {
      int index = -1;
      int A1index = 2; // A12, A13 jne
      int A2B2index = 0; // tavoitteena A2, A22, A23 tai B2, B22, B23 jne
      try {
        List<Map.Entry<String, List<AvainArvoDTO>>> valuesForMatchingLangs =
            grouped.entrySet().stream()
                .filter(
                    entry ->
                        entry.getValue().stream()
                            .filter(dto -> dto.getAvain().contains("oppiaine"))
                            .findFirst()
                            .map(dto -> StringUtils.substringAfterLast(dto.getArvo(), "-"))
                            .orElse("")
                            .toUpperCase()
                            .equals(aineKey))
                .sorted(Map.Entry.comparingByKey())
                .collect(Collectors.toList());

        LOG.info("Handling aineKey {} with dtos: {}", aineKey, valuesForMatchingLangs);

        for (Map.Entry<String, List<AvainArvoDTO>> entry : valuesForMatchingLangs) {
          index++;
          LOG.info("Handling entry {}, index {}", entry, index);
          String kieli =
              entry.getValue().stream()
                  .filter(dto -> dto.getAvain().startsWith("oppimaara"))
                  .findFirst()
                  .map(AvainArvoDTO::getArvo)
                  .orElse("")
                  .toUpperCase();
          String arvosana =
              entry.getValue().stream()
                  .filter(dto -> dto.getAvain().startsWith("arvosana"))
                  .findFirst()
                  .map(dto -> StringUtils.substringAfterLast(dto.getArvo(), "-"))
                  .orElse("")
                  .toUpperCase();
          Integer.parseInt(arvosana); // Just check that the arvosana correctly parses as Integer.
          String valSuffix = "";
          String langPrefix = aineKey;

          if (aineKey.equals("A1")) {
            // Muunnetaan "valinnainen a1-kieli" pakolliseksi, avain muotoon PK_A12, PK_A13 jne
            langPrefix = aineKey + A1index;
            LOG.warn(
                "Muunnetaan hakemuksen {} valinnainen A1-kieli {} pakolliseksi avaimelle {}",
                hakemusOid,
                kieli,
                prefix + langPrefix);
            A1index++;
          } else if (aineKey.equals("A2") || aineKey.equals("B2")) {
            A2B2index++;
            if (A2B2index > 1) {
              langPrefix = aineKey + A2B2index;
              LOG.warn(
                  "Muunnetaan hakemuksen {} valinnainen kieli {} pakolliseksi avaimelle {}",
                  hakemusOid,
                  kieli,
                  prefix + langPrefix);
            }
          } else if (index > 0) {
            valSuffix = "_VAL" + index;
          }

          if (!arvosana.isEmpty() && !kieli.isEmpty() && !aineKey.isEmpty()) {
            String arvosanaKey = prefix + langPrefix + valSuffix;
            r.add(new AvainArvoDTO(arvosanaKey, arvosana));
            if (index == 0
                || aineKey.equals("A1")
                || aineKey.equals("A2")
                || aineKey.equals("B2")) {
              String oppiaineKey = arvosanaKey + "_OPPIAINE";
              r.add(new AvainArvoDTO(oppiaineKey, kieli));
            }
          } else {
            LOG.warn("Tyhjä arvo kielelle {}: {}", aineKey, entry);
          }
        }
      } catch (Exception e) {
        LOG.error(
            "Hakemuksen {} valinnaisen kielen parsiminen ei onnistunut: {}",
            hakemusOid,
            aineKey,
            e);
      }
    }

    return r;
  }

  public static List<AvainArvoDTO> convertAtaruArvosanas(
      Map<String, AvainArvoDTO> keyValues, String hakemusOid) {
    List<AvainArvoDTO> r = new ArrayList<>();
    r.addAll(convertValinnaisetKielet(keyValues, hakemusOid));
    for (Map.Entry<String, AvainArvoDTO> entry : keyValues.entrySet()) {
      String key = entry.getKey();
      if (key.startsWith("arvosana") && !key.contains("valinnainen-kieli")) {
        try {
          String aineKey = StringUtils.substringBetween(key, "arvosana-", "_group");
          if (aineKey.equals("A")) {
            aineKey = "AI";
          }
          String valSuffix = "";
          if (!key.endsWith("0")) {
            valSuffix = "_VAL" + key.substring(key.length() - 1);
          }
          String aine = prefix + aineKey + valSuffix;
          String arvosana = StringUtils.substringAfterLast(entry.getValue().getArvo(), "-");
          LOG.debug("key " + key + ", result " + aineKey + valSuffix);
          Integer.parseInt(arvosana); // Just check that the arvosana correctly parses as Integer.
          r.add(new AvainArvoDTO(aine, arvosana));
        } catch (Exception e) {
          LOG.warn(
              "Virhe ({}) parsittaessa hakemuksen {} ataruarvosanaa {}. Jatketaan normaalisti, mutta tätä arvosanaa ei oteta huomioon.",
              e.getMessage(),
              hakemusOid,
              entry);
        }
      } else if (key.startsWith("oppimaara") && !key.contains("valinnainen-kieli")) {
        LOG.info("handling oppiaine: {}", entry);
        // oppimaara-kieli-B1_group0
        try {
          String kieliKey = StringUtils.substringBetween(key, "oppimaara-kieli-", "_group");
          boolean isAidinkieli = StringUtils.startsWith(key, "oppimaara-a_group");
          String arvo = entry.getValue().getArvo();
          if (isAidinkieli) {
            kieliKey = "AI";
            arvo = convertAtaruAidinkieliValue(arvo);
          }
          // Kerätään oppimäärätieto vain kerran, "group0"-loppuiselta avaimelta.
          if (kieliKey != null && !arvo.isEmpty() && key.endsWith("0")) {
            String oppiaineKey = prefix + kieliKey + "_OPPIAINE";
            r.add(new AvainArvoDTO(oppiaineKey, arvo));
          }
        } catch (Exception e) {
          LOG.warn(
              "Virhe ({}) parsittaessa hakemuksen {} ataruoppiainetta {}. Jatketaan normaalisti, mutta tätä oppiainetietoa ei oteta huomioon.",
              e.getMessage(),
              hakemusOid,
              entry);
        }
      }
    }
    return r;
  }
}
