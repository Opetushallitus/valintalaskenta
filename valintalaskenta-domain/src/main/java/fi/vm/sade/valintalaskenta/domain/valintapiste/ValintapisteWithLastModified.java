package fi.vm.sade.valintalaskenta.domain.valintapiste;

import fi.vm.sade.valintalaskenta.domain.dto.valintapiste.Osallistumistieto;
import java.sql.Timestamp;

public record ValintapisteWithLastModified(
    String hakemusOid,
    String tunniste,
    String arvo,
    Osallistumistieto osallistuminen,
    String tallettaja,
    Timestamp lastModified) {}
