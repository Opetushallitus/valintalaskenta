package fi.vm.sade.valinta.kooste.excel.arvo;

import fi.vm.sade.valinta.kooste.excel.ArvoTyyppi;

public class Arvo {
  private final ArvoTyyppi tyyppi;

  public Arvo(ArvoTyyppi tyyppi) {
    this.tyyppi = tyyppi;
  }

  public ArvoTyyppi getTyyppi() {
    return tyyppi;
  }

  public MonivalintaArvo asMonivalintaArvo() {
    return (MonivalintaArvo) this;
  }

  public NumeroArvo asNumeroArvo() {
    return (NumeroArvo) this;
  }

  public TekstiArvo asTekstiArvo() {
    return (TekstiArvo) this;
  }
}
