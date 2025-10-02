package fi.vm.sade.valintalaskenta.domain.dto.valintapiste;

public record ValintapisteDTO(
    String hakemusOid,
    String tunniste,
    String arvo,
    Osallistumistieto osallistuminen,
    String tallettaja) {}
