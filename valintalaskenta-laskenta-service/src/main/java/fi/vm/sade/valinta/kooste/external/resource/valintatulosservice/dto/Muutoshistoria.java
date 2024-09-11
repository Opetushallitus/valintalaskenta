package fi.vm.sade.valinta.kooste.external.resource.valintatulosservice.dto;

import java.time.OffsetDateTime;
import java.util.List;

public class Muutoshistoria {

  private List<Change> changes;
  private OffsetDateTime timestamp;

  public List<Change> getChanges() {
    return changes;
  }

  public OffsetDateTime getTimestamp() {
    return timestamp;
  }
}
