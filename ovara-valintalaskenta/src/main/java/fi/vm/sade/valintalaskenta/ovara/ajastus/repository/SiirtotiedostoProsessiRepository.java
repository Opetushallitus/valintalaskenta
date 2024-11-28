package fi.vm.sade.valintalaskenta.ovara.ajastus.repository;

import fi.vm.sade.valintalaskenta.ovara.ajastus.SiirtotiedostoProsessi;
import java.util.UUID;
import org.springframework.data.jdbc.repository.query.Query;
import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface SiirtotiedostoProsessiRepository
    extends CrudRepository<SiirtotiedostoProsessi, UUID> {

  @Query(
      "SELECT execution_uuid, window_start, window_end, run_start, run_end, info, success, error_message from siirtotiedosto where success order by run_end desc limit 1")
  SiirtotiedostoProsessi findLatestSuccessful();
}
