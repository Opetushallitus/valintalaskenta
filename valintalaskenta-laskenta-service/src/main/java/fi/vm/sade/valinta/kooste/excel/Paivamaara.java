package fi.vm.sade.valinta.kooste.excel;

import fi.vm.sade.valinta.kooste.util.Formatter;
import java.util.Date;

public class Paivamaara extends Solu {
  private final Date paivamaara;
  private final boolean muokattava;
  private final int preferoituleveys;

  public Paivamaara() {
    this.paivamaara = null;
    this.muokattava = false;
    this.preferoituleveys = 0;
  }

  public Date getPaivamaara() {
    return paivamaara;
  }

  public Paivamaara(Date paivamaara) {
    this.paivamaara = paivamaara;
    this.muokattava = false;
    this.preferoituleveys = 0;
  }

  @Override
  public int preferoituLeveys() {
    return preferoituleveys;
  }

  public boolean isMuokattava() {
    return muokattava;
  }

  @Override
  public boolean isTyhja() {
    return paivamaara == null;
  }

  public boolean isNumero() {
    return false;
  }

  public boolean isTeksti() {
    return false;
  }

  @Override
  public Monivalinta toMonivalinta() {
    throw new RuntimeException("Päivämäärää ei voi muuttaa monivalintakentäksi!");
  }

  @Override
  public Teksti toTeksti() {
    return new Teksti(Formatter.paivamaara(paivamaara));
  }

  @Override
  public boolean isTasausOikealle() {
    return true;
  }

  @Override
  public Numero toNumero() {
    throw new RuntimeException("Päivämäärää ei voi muuttaa numeroksi!");
  }
}
