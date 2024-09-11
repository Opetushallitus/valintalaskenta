package fi.vm.sade.valinta.kooste.hakemus.dto;

import fi.vm.sade.valinta.kooste.util.HakemusWrapper;
import java.util.Collection;
import org.apache.commons.lang.StringUtils;

public class Yhteystiedot {

  public static final String SAHKOPOSTI = "sähköposti";
  public static final String MATKAPUHELINNUMERO = "matkapuhelinnumero";
  private String sahkoposti;
  private Collection<String> puhelinnumerot;

  private Yhteystiedot() {
    this.sahkoposti = null;
    this.puhelinnumerot = null;
  }

  private Yhteystiedot(String sahkoposti, Collection<String> puhelinnumerot) {
    this.sahkoposti = sahkoposti;
    this.puhelinnumerot = puhelinnumerot;
  }

  public Collection<String> getPuhelinnumerot() {
    return puhelinnumerot;
  }

  public String getPuhelinnumerotAsString() {
    if (puhelinnumerot == null || puhelinnumerot.isEmpty()) {
      return StringUtils.EMPTY;
    } else {
      StringBuilder b = new StringBuilder();
      for (String puhelinnumero : puhelinnumerot) {
        if (puhelinnumero != null && !StringUtils.isEmpty(puhelinnumero)) {
          b.append(puhelinnumero).append(" ");
        }
      }
      return b.toString().trim();
    }
  }

  public String getSahkoposti() {
    return StringUtils.trimToEmpty(sahkoposti);
  }

  public static Yhteystiedot yhteystiedotHakemukselta(HakemusWrapper hakemus) {
    if (hakemus != null) {
      return new Yhteystiedot(hakemus.getSahkopostiOsoite(), hakemus.getPuhelinnumerot());
    }
    return new Yhteystiedot();
  }

  @Override
  public String toString() {
    StringBuilder b = new StringBuilder();
    if (sahkoposti != null) {
      b.append(sahkoposti).append(" ");
    }
    for (String puhelinnumero : puhelinnumerot) {
      b.append(puhelinnumero).append(" ");
    }
    return b.toString().trim();
  }
}
