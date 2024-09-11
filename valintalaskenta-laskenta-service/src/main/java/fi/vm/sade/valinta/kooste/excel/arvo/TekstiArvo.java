package fi.vm.sade.valinta.kooste.excel.arvo;

import fi.vm.sade.valinta.kooste.excel.ArvoTyyppi;
import fi.vm.sade.valinta.kooste.excel.Excel;
import fi.vm.sade.valinta.kooste.excel.Teksti;

public class TekstiArvo extends Arvo {
  private final String teksti;
  private final boolean nimike;
  private final boolean editoitava;
  private final int ulottuvuus;

  public TekstiArvo(String teksti) {
    this(teksti, true);
  }

  public TekstiArvo(String teksti, boolean nimike) {
    this(teksti, nimike, false);
  }

  public TekstiArvo(String teksti, boolean nimike, boolean editoitava) {
    this(teksti, nimike, editoitava, 1);
  }

  public TekstiArvo(String teksti, boolean nimike, boolean editoitava, int ulottuvuus) {
    super(ArvoTyyppi.NIMIKE);
    this.teksti = teksti;
    this.nimike = nimike;
    this.editoitava = editoitava;
    this.ulottuvuus = ulottuvuus;
  }

  @Override
  public String toString() {
    return teksti;
  }

  public Teksti asTeksti() {
    int preferoituleveys = 0;
    if (!nimike) {
      preferoituleveys = Excel.VAKIO_LEVEYS / 2;
    }
    return new Teksti(
        teksti, true, false, teksti == null, preferoituleveys, ulottuvuus, editoitava);
    // Excel.VAKIO_LEVEYS
    // / 2);
  }

  private static final TekstiArvo TYHJA_EI_NIMIKE = new TekstiArvo(null, false);

  public static TekstiArvo editoimatonTyhja() {
    return TYHJA_EI_NIMIKE;
  }
}
