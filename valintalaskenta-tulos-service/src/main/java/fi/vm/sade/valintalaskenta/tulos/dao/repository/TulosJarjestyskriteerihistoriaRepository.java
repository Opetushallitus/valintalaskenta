package fi.vm.sade.valintalaskenta.tulos.dao.repository;

import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteerihistoria;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import org.springframework.data.jdbc.repository.query.Query;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.query.Param;

public interface TulosJarjestyskriteerihistoriaRepository
    extends CrudRepository<Jarjestyskriteerihistoria, Long> {

  @Query("SELECT * FROM Jarjestyskriteerihistoria ORDER BY id ASC LIMIT 100 FOR UPDATE SKIP LOCKED")
  List<Jarjestyskriteerihistoria> fetchOldest();

  @Query(
      "select DISTINCT ON (jkh.tunniste) * from Jarjestyskriteerihistoria jkh WHERE jkh.tunniste in (:tunnisteet) ORDER BY jkh.tunniste, jkh.id DESC")
  List<Jarjestyskriteerihistoria> findLatestByTunnisteet(
      @Param("tunnisteet") List<UUID> tunnisteet);

  @Query(
      "select * from Jarjestyskriteerihistoria jkh WHERE jkh.tunniste = :tunniste ORDER BY id DESC LIMIT 1")
  Optional<Jarjestyskriteerihistoria> findLatestByTunniste(@Param("tunniste") UUID tunniste);
}
