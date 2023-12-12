package fi.vm.sade.valintalaskenta.laskenta.dao;

import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteerihistoria;
import java.util.UUID;

public interface JarjestyskriteerihistoriaDAO {

  void create(Jarjestyskriteerihistoria jarjestyskriteerihistoria);

  void delete(UUID id);

  Jarjestyskriteerihistoria hae(UUID id);
}
