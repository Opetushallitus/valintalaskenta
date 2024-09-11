package fi.vm.sade.valinta.kooste.external.resource.valintapiste.dto;

import java.util.List;
import java.util.Optional;

public class PisteetWithLastModified {

  public final Optional<String> lastModified;
  public final List<Valintapisteet> valintapisteet;

  public PisteetWithLastModified(
      Optional<String> lastModified, List<Valintapisteet> valintapisteet) {
    this.lastModified = lastModified;
    this.valintapisteet = valintapisteet;
  }
}
