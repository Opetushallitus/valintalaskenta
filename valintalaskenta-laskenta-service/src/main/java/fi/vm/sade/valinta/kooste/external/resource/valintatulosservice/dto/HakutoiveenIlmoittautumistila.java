package fi.vm.sade.valinta.kooste.external.resource.valintatulosservice.dto;

import scala.Option;

public class HakutoiveenIlmoittautumistila {
  Ilmoittautumisaika ilmoittautumisaika;
  Option<Ilmoittautumistapa> ilmoittautumistapa;
  String ilmoittautumistila;
  Boolean ilmoittauduttavissa;

  public HakutoiveenIlmoittautumistila(
      Ilmoittautumisaika ilmoittautumisaika,
      Option<Ilmoittautumistapa> ilmoittautumistapa,
      String ilmoittautumistila,
      Boolean ilmoittauduttavissa) {
    this.ilmoittautumisaika = ilmoittautumisaika;
    this.ilmoittautumistapa = ilmoittautumistapa;
    this.ilmoittautumistila = ilmoittautumistila;
    this.ilmoittauduttavissa = ilmoittauduttavissa;
  }
}
