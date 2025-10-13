package fi.vm.sade.valintalaskenta.laskenta.dao;

import fi.vm.sade.valintalaskenta.domain.valintapiste.Valintapiste;
import fi.vm.sade.valintalaskenta.domain.valintapiste.ValintapisteWithLastModified;
import java.time.ZonedDateTime;
import java.util.List;
import java.util.Optional;

public interface ValintapisteDAO {
  List<Valintapiste> findByHakemusId(String hakemusOid);

  List<Valintapiste> findValintapisteetForHakemukset(List<String> hakemusOids);

  List<ValintapisteWithLastModified> findValintapisteBulkByTimerange(
      ZonedDateTime start, ZonedDateTime end, int limit, int offset);

  Optional<ZonedDateTime> lastModifiedForHakemukset(List<String> hakemusOids);

  Optional<ZonedDateTime> lastModifiedASDF();

  List<String> modifiedSinceHakemukset(List<String> hakemusOids, String unmodifiedSince);

  void upsertValintapiste(Valintapiste valintapiste);
}
