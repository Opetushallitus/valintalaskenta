package fi.vm.sade.valinta.kooste.external.resource.valintatulosservice.dto;

import java.util.Map;

public class Ilmoittautumistapa {
  private Map<String, String> nimi;
  private String url;

  public Ilmoittautumistapa(Map<String, String> nimi, String url) {
    this.nimi = nimi;
    this.url = url;
  }
}
