package fi.vm.sade.valinta.kooste.util;

import com.google.common.collect.Sets;
import fi.vm.sade.koodisto.service.types.common.KieliType;
import java.util.Collection;
import java.util.Set;

public class KieliUtil {

  public static final String PREFEROITUKIELIKOODI = "preferoitukielikoodi";
  private static final String SUOMI_KIELI = "SUOMI";
  private static final String RUOTSI_KIELI = "RUOTSI";
  private static final String ENGLANTI_KIELI = "ENGLANTI";

  private static final String SUOMI_KOODI = "KIELI_FI";
  private static final String RUOTSI_KOODI = "KIELI_SV";
  private static final String ENGLANTI_KOODI = "KIELI_EN";

  public static final String SUOMI = "FI";
  public static final String RUOTSI = "SV";
  public static final String RUOTSI_2 = "SE";
  public static final String ENGLANTI = "EN";

  /**
   * Kielikoodit pitaisi tulla muodossa "FI","SE", "EN". Kaytannossa voi tulla vaikka "fi,se,en" tai
   * "fi_FI" tai "ruotsi".
   *
   * @param kielikoodi kielikoodi palveluntarjoajan omalla enkoodauksella
   * @return normalisoitu kielikoodi muotoa "FI","SE","EN"
   */
  public static String normalisoiKielikoodi(String kielikoodi) {
    if (kielikoodi == null) {
      return SUOMI; // Oletuksena suomi
    } else if (SUOMI_KIELI.equalsIgnoreCase(kielikoodi)) {
      return SUOMI;
    } else if (RUOTSI_KIELI.equalsIgnoreCase(kielikoodi)) {
      return RUOTSI;
    } else if (ENGLANTI_KIELI.equalsIgnoreCase(kielikoodi)) {
      return ENGLANTI;
    } else if (SUOMI_KOODI.equalsIgnoreCase(kielikoodi)) {
      return SUOMI;
    } else if (RUOTSI_KOODI.equalsIgnoreCase(kielikoodi)) {
      return RUOTSI;
    } else if (ENGLANTI_KOODI.equalsIgnoreCase(kielikoodi)) {
      return ENGLANTI;

    } else if (SUOMI.equalsIgnoreCase(kielikoodi)) {
      return SUOMI;
    } else if (RUOTSI.equalsIgnoreCase(kielikoodi)) {
      return RUOTSI;
    } else if (ENGLANTI.equalsIgnoreCase(kielikoodi)) {
      return ENGLANTI;
    }
    // Ei tarkkaa osumaa. Kokeillaan nimenosilla ja preferoidaan suomea,
    // sitten ruotsia ja lopuksi englanti
    String uppercaseKielikoodi = kielikoodi.toUpperCase();
    if (uppercaseKielikoodi.contains(SUOMI)) {
      return SUOMI;
    } else if (uppercaseKielikoodi.contains(RUOTSI) || uppercaseKielikoodi.contains(RUOTSI_2)) {
      return RUOTSI;
    }
    return ENGLANTI; // Tuntematon ulkomaa => englanti
  }

  public static String preferoi(Collection<String> normalisoidutKielet) {
    Set<String> k = Sets.newHashSet(normalisoidutKielet);
    if (k.contains(SUOMI)) {
      return SUOMI;
    } else if (k.contains(RUOTSI)) {
      return RUOTSI;
    } else {
      return ENGLANTI;
    }
  }

  public static KieliType kieliToKieliType(String kieli) {
    switch (kieli) {
      case SUOMI:
        return KieliType.FI;
      case RUOTSI:
      case RUOTSI_2:
        return KieliType.SV;
      case ENGLANTI:
        return KieliType.EN;
      default:
        return KieliType.FI;
    }
  }
}
