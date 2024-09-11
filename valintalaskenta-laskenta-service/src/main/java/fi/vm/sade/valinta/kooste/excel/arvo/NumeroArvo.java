package fi.vm.sade.valinta.kooste.excel.arvo;

import fi.vm.sade.valinta.kooste.excel.ArvoTyyppi;
import fi.vm.sade.valinta.kooste.excel.Excel;
import fi.vm.sade.valinta.kooste.excel.Numero;
import org.apache.commons.lang.StringUtils;

public class NumeroArvo extends Arvo {
  private final Number numero;
  private final Number min;
  private final Number max;

  public NumeroArvo(Number numero) {
    super(ArvoTyyppi.NUMERO);
    this.numero = numero;
    this.min = null;
    this.max = null;
  }

  public NumeroArvo(Number numero, Number min, Number max) {
    super(ArvoTyyppi.NUMERO);
    this.numero = numero;
    this.min = min;
    this.max = max;
  }

  public Numero asNumero() {
    return new Numero(numero, min, max, true, Excel.VAKIO_LEVEYS / 2);
  }

  @Override
  public String toString() {
    if (numero == null) {
      return StringUtils.EMPTY;
    }
    return numero.toString();
  }
}
