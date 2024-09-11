package fi.vm.sade.valinta.kooste.util;

import fi.vm.sade.valinta.kooste.erillishaku.excel.Sukupuoli;
import java.util.Set;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class HenkilotunnusTarkistusUtil {
  private static final Logger LOG = LoggerFactory.getLogger(HenkilotunnusTarkistusUtil.class);
  private static final char[] TARKISTUSMERKKI = {
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F', 'H', 'J', 'K',
    'L', 'M', 'N', 'P', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y'
  };

  private static final Set<String> VALIMERKIT =
      Set.of("-", "A", "B", "C", "D", "E", "F", "U", "V", "W", "X", "Y");

  public static boolean tarkistaHenkilotunnus(String henkilotunnus) {
    if (henkilotunnus == null || henkilotunnus.length() != 11) {
      return false; // ei vastaa henkilötunnusta sisällön koolta
    }
    if (!VALIMERKIT.contains(henkilotunnus.substring(6, 7))) {
      return false;
    }
    String syntymaaika = henkilotunnus.substring(0, 6);
    String numero = henkilotunnus.substring(7, 10);
    int tarkistettavaNumero;
    try {
      tarkistettavaNumero = Integer.parseInt(syntymaaika + numero);
    } catch (NumberFormatException e) {
      return false;
    }
    return TARKISTUSMERKKI[(tarkistettavaNumero % 31)] == henkilotunnus.charAt(10);
  }

  public static Sukupuoli palautaSukupuoli(String henkilotunnus) {
    if (!tarkistaHenkilotunnus(henkilotunnus)) {
      return Sukupuoli.EI_SUKUPUOLTA;
    } else if (Integer.parseInt("" + henkilotunnus.charAt(9)) % 2 == 0) {
      return Sukupuoli.NAINEN;
    } else {
      return Sukupuoli.MIES;
    }
  }
}
