package fi.vm.sade.valintalaskenta.domain.dto.siirtotiedosto;

import java.time.ZoneId;
import java.time.format.DateTimeFormatter;

public abstract class SiirtotiedostoConstants {
  public static final String SIIRTOTIEDOSTO_DATETIME_FORMAT = "yyyy-MM-dd'T'HH:mm:ss";
  public static final ZoneId SIIRTOTIEDOSTO_TIMEZONE = ZoneId.of("Europe/Helsinki");
  public static DateTimeFormatter SIIRTOTIEDOSTO_DATETIME_FORMATTER =
      DateTimeFormatter.ofPattern(SIIRTOTIEDOSTO_DATETIME_FORMAT).withZone(SIIRTOTIEDOSTO_TIMEZONE);
}
