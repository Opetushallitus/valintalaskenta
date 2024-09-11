package fi.vm.sade.valinta.kooste.excel.arvo;

import java.util.Collection;
import org.apache.commons.lang3.BooleanUtils;

public class BooleanArvo extends MonivalintaArvo {

  public BooleanArvo(
      String arvo, Collection<String> valinnat, String tosi, String epatosi, String tyhja) {
    super(asString(asBoolean(arvo), tosi, epatosi, tyhja), valinnat);
  }

  public BooleanArvo(
      Boolean arvo, Collection<String> valinnat, String tosi, String epatosi, String tyhja) {
    super(asString(arvo, tosi, epatosi, tyhja), valinnat);
  }

  private static String asString(Boolean arvo, String tosi, String epatosi, String tyhja) {
    if (Boolean.TRUE.equals(arvo)) {
      return tosi;
    } else if (Boolean.FALSE.equals(arvo)) {
      return epatosi;
    } else {
      return tyhja;
    }
  }

  private static Boolean asBoolean(String arvo) {
    return BooleanUtils.toBooleanObject(arvo);
    // return Boolean.valueOf(arvo);
  }
}
