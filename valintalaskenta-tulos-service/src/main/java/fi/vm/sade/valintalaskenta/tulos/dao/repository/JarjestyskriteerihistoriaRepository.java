package fi.vm.sade.valintalaskenta.tulos.dao.repository;

import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteerihistoria;
import java.util.List;
import java.util.UUID;
import org.springframework.data.jdbc.repository.query.Query;
import org.springframework.data.repository.CrudRepository;

public interface JarjestyskriteerihistoriaRepository
    extends CrudRepository<Jarjestyskriteerihistoria, UUID> {

  @Query("select * from Jarjestyskriteerihistoria ORDER BY created_at ASC LIMIT 100")
  List<Jarjestyskriteerihistoria> fetchOldest();
}
