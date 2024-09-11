package fi.vm.sade.valinta.kooste.excel;

import java.util.Arrays;
import java.util.Collection;

public class Numero extends Solu {
  private final Number numero;
  private final Number min;
  private final Number max;
  private final boolean muokattava;
  private final int preferoituleveys;

  public Numero() {
    this.min = null;
    this.max = null;
    this.numero = null;
    this.muokattava = false;
    this.preferoituleveys = 0;
  }

  public Number getMax() {
    return max;
  }

  public Number getMin() {
    return min;
  }

  public Number getNumero() {
    if (numero == null) {
      return null;
    }
    return numero;
  }

  public boolean hasArvovali() {
    return min != null && max != null;
  }

  public Collection<Number> asArvovali() {
    return Arrays.asList(min, max);
  }

  public Numero(Number numero) {
    this.numero = numero;
    this.muokattava = false;
    this.min = null;
    this.max = null;
    this.preferoituleveys = 0;
  }

  public Numero(Number numero, boolean muokattava) {
    super(null);
    this.numero = numero;
    this.muokattava = muokattava;
    this.min = null;
    this.max = null;
    this.preferoituleveys = 0;
  }

  public Numero(Number numero, Number min, Number max, boolean muokattava) {
    super(null);
    this.numero = numero;
    this.muokattava = muokattava;
    this.min = min;
    this.max = max;
    this.preferoituleveys = 0;
  }

  public Numero(Number numero, Number min, Number max, boolean muokattava, int preferoituleveys) {
    super(null);
    this.numero = numero;
    this.muokattava = muokattava;
    this.min = min;
    this.max = max;
    this.preferoituleveys = preferoituleveys;
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
    return numero == null;
  }

  public boolean isNumero() {
    return true;
  }

  public boolean isTeksti() {
    return false;
  }

  @Override
  public Monivalinta toMonivalinta() {
    throw new RuntimeException("Numeroa ei voi muuttaa monivalintakentäksi!");
  }

  @Override
  public Teksti toTeksti() {
    return new Teksti("" + numero);
  }

  @Override
  public boolean isTasausOikealle() {
    return true;
  }

  @Override
  public Numero toNumero() {
    return this;
  }
}
