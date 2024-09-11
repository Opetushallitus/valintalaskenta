package fi.vm.sade.valinta.kooste.external.resource.valintatulosservice.dto;

public enum Maksuntila {
  MAKSAMATTA,
  MAKSETTU,
  VAPAUTETTU;

  public static Maksuntila fromString(String s) {
    try {
      return Maksuntila.valueOf(s);
    } catch (IllegalArgumentException e) {
      return Maksuntila.MAKSAMATTA;
    }
  }
}
