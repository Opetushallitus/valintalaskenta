package fi.vm.sade.valintalaskenta.domain.dto.siirtotiedosto;

import java.util.List;

public record SiirtotiedostoResult(List<String> keys, int total, boolean success) {
  public SiirtotiedostoResult(List<String> keys, int total) {
    this(keys, total, true);
  }
}
