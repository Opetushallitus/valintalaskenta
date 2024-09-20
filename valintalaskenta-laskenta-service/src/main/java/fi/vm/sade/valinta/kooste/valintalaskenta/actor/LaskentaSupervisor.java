package fi.vm.sade.valinta.kooste.valintalaskenta.actor;

import fi.vm.sade.valinta.kooste.valintalaskenta.dto.Laskenta;

import java.util.List;
import java.util.Optional;

public interface LaskentaSupervisor {
  void workAvailable();

  void ready(String uuid);

  void fetchAndStartLaskenta();

  Optional<Laskenta> fetchLaskenta(String uuid);

  List<Laskenta> runningLaskentas();
}
