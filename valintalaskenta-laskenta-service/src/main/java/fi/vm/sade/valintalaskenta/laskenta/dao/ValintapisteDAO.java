package fi.vm.sade.valintalaskenta.laskenta.dao;

import fi.vm.sade.valintalaskenta.domain.valintapiste.Valintapiste;
import fi.vm.sade.valintalaskenta.domain.valintapiste.ValintapisteWithLastModified;
import java.time.ZonedDateTime;
import java.util.List;

public interface ValintapisteDAO {
  List<Valintapiste> findValintapisteetForHakemukset(List<String> hakemusOids);

  List<ValintapisteWithLastModified> findValintapisteBulkByTimerange(
      ZonedDateTime start, ZonedDateTime end, int limit, int offset);

  ZonedDateTime lastModifiedForHakemukset(List<String> hakemusOids);

  ZonedDateTime lastModifiedASDF();

  List<String> modifiedSinceHakemukset(List<String> hakemusOids, ZonedDateTime unmodifiedSince);

  void upsertValintapiste(Valintapiste valintapiste);
}
