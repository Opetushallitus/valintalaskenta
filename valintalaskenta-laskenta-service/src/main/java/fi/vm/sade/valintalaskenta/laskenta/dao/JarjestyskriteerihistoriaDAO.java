package fi.vm.sade.valintalaskenta.laskenta.dao;

import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteerihistoria;
import java.util.List;
import java.util.UUID;

public interface JarjestyskriteerihistoriaDAO {

  void create(Jarjestyskriteerihistoria jarjestyskriteerihistoria);

  void createVersionWithUpdate(UUID tunniste);

  void delete(Long id);

  Jarjestyskriteerihistoria hae(UUID id);

  List<Jarjestyskriteerihistoria> fetchOldest();
}
