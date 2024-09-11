package fi.vm.sade.valinta.kooste.dto;

import com.google.common.collect.ImmutableMap;
import java.util.Map;

/**
 * Geneerinen vastaus viesti koostepalvelulta. Modaalisen dialogin esittämiseen erinäisistä
 * onnistumis/virhe skenaarioista!
 */
public class Vastaus {

  private String viesti;
  private String latausUrl;
  private Map<String, Object> lisatiedot;

  private Vastaus() {}

  private Vastaus(String latausUrl) {
    this.latausUrl = latausUrl;
  }

  private Vastaus(String latausUrl, Map<String, Object> lisatiedot) {
    this.latausUrl = latausUrl;
    this.lisatiedot = lisatiedot;
  }

  public String getLatausUrl() {
    return latausUrl;
  }

  public Map<String, Object> getLisatiedot() {
    return lisatiedot;
  }

  public String getViesti() {
    return viesti;
  }

  public static Vastaus uudelleenOhjaus(String uudelleenOhjausUrl) {
    return new Vastaus(uudelleenOhjausUrl);
  }

  public static Vastaus laskennanSeuraus(String uudelleenOhjausUrl, boolean luotiinkoUusiLaskenta) {
    return new Vastaus(
        uudelleenOhjausUrl, ImmutableMap.of("luotiinkoUusiLaskenta", luotiinkoUusiLaskenta));
  }

  public static Vastaus virhe(String virheViesti) {
    Vastaus vastaus = new Vastaus();
    vastaus.viesti = virheViesti;
    return vastaus;
  }

  public static Vastaus viesti(String viesti) {
    Vastaus vastaus = new Vastaus();
    vastaus.viesti = viesti;
    return vastaus;
  }
}
