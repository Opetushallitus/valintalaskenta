package fi.vm.sade.valintalaskenta.domain.dto.valintapiste;

public record Pistetieto(
    String tunniste, String arvo, Osallistumistieto osallistuminen, String tallettaja) {}
