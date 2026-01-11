package fi.vm.sade.valintalaskenta.tulos.dao;

import fi.vm.sade.valintalaskenta.domain.valintapiste.Valintapiste;
import fi.vm.sade.valintalaskenta.domain.valintapiste.ValintapisteWithLastModified;
import java.time.LocalDateTime;
import java.time.ZonedDateTime;
import java.util.Collection;
import java.util.List;
import java.util.Optional;

public interface ValintapisteDAO {
  List<Valintapiste> findByHakemusId(String hakemusOid);

  List<Valintapiste> findValintapisteetForHakemukset(Collection<String> hakemusOids);

  List<ValintapisteWithLastModified> findValintapisteBulkByTimerange(
      LocalDateTime start, LocalDateTime end, int limit, int offset);

  List<String> findDeleted(LocalDateTime start, LocalDateTime end);

  Optional<ZonedDateTime> lastModifiedForHakemukset(Collection<String> hakemusOids);

  List<String> modifiedSinceHakemukset(List<String> hakemusOids, String unmodifiedSince);

  void upsertValintapiste(Valintapiste valintapiste);
}
