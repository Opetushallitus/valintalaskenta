package fi.vm.sade.valinta.kooste.util;

import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.Date;
import java.util.Locale;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.time.FastDateFormat;

public class Formatter {
  public static final FastDateFormat PVMFORMATTER = FastDateFormat.getInstance("dd.MM.yyyy HH:mm");
  public static final DecimalFormat NUMERO_FORMAATTI =
      (DecimalFormat) NumberFormat.getInstance(new Locale("FI"));
  public static final String ARVO_EROTIN = " / ";
  public static final String ARVO_VALI = " ";
  public static final String ARVO_VAKIO = "-";

  static {
    NUMERO_FORMAATTI.setMinimumFractionDigits(0);
    NUMERO_FORMAATTI.setMaximumFractionDigits(Integer.MAX_VALUE);
    NUMERO_FORMAATTI.setNegativePrefix("-");
  }

  public static String suomennaNumero(BigDecimal arvo) {
    return suomennaNumero(arvo, StringUtils.EMPTY);
  }

  public static String suomennaNumero(Number arvo) {
    if (arvo == null) {
      return StringUtils.EMPTY;
    } else {
      return NUMERO_FORMAATTI.format(arvo);
    }
  }

  public static String paivamaara(Date paivamaara) {
    if (paivamaara == null) {
      return StringUtils.EMPTY;
    } else {
      return PVMFORMATTER.format(paivamaara);
    }
  }

  public static String suomennaNumero(BigDecimal arvo, String vakioArvo) {
    if (arvo == null) {
      return vakioArvo;
    }
    return NUMERO_FORMAATTI.format(arvo);
  }

  public static String suomennaNumero(Integer arvo, String vakioArvo) {
    if (arvo == null) {
      return vakioArvo;
    }
    return NUMERO_FORMAATTI.format(arvo);
  }

  public static String suomennaNumero(Integer arvo) {
    return suomennaNumero(arvo, StringUtils.EMPTY);
  }

  public static String humanReadableByteCount(long bytes) {
    return humanReadableByteCount(bytes, true);
  }

  public static String humanReadableByteCount(long bytes, boolean si) {
    int unit = si ? 1000 : 1024;
    if (bytes < unit) return bytes + " B";
    int exp = (int) (Math.log(bytes) / Math.log(unit));
    String pre = (si ? "kMGTPE" : "KMGTPE").charAt(exp - 1) + (si ? "" : "i");
    return String.format("%.1f %sB", bytes / Math.pow(unit, exp), pre);
  }
}
