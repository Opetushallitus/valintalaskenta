package fi.vm.sade.valintalaskenta.tulos.dao;

import fi.vm.sade.valintalaskenta.domain.siirtotiedosto.Poistettu;
import java.time.LocalDateTime;
import java.util.Collection;
import java.util.List;
import java.util.UUID;

public interface PoistettuDAO {
  public static enum EntityType {
    VALINNANVAIHE,
    VALINTATAPAJONO,
    JONOSIJA,
    VALINTAKOE_OSALLISTUMINEN,
    HAKUTOIVE,
    VALINTAKOE_VALINNANVAIHE,
    VALINTAKOE
  }

  List<Poistettu> findPoistetut(
      EntityType entityType, LocalDateTime startDateTime, LocalDateTime endDateTime);

  List<Poistettu> findParents(EntityType parentType, Collection<UUID> ids);
}
