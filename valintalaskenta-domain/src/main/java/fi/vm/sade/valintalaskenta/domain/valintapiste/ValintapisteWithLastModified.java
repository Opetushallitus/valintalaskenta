package fi.vm.sade.valintalaskenta.domain.valintapiste;

import fi.vm.sade.valintalaskenta.domain.dto.valintapiste.Osallistumistieto;
import java.sql.Timestamp;
import java.time.ZoneId;
import java.time.ZonedDateTime;

public record ValintapisteWithLastModified(
    String hakemusOid,
    String tunniste,
    String arvo,
    Osallistumistieto osallistuminen,
    String tallettaja,
    ZonedDateTime lastModified) {
  public ValintapisteWithLastModified(
      String hakemusOid,
      String tunniste,
      String arvo,
      Osallistumistieto osallistuminen,
      String tallettaja,
      Timestamp lastModified) {
    this(
        hakemusOid,
        tunniste,
        arvo,
        osallistuminen,
        tallettaja,
        lastModified.toInstant().atZone(ZoneId.of("UTC")));
  }
}
