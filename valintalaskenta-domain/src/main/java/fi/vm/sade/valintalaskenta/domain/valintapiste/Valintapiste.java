package fi.vm.sade.valintalaskenta.domain.valintapiste;

import fi.vm.sade.valintalaskenta.domain.dto.valintapiste.Osallistumistieto;
import fi.vm.sade.valintalaskenta.domain.dto.valintapiste.Pistetieto;

public record Valintapiste(
    String hakemusOid,
    String tunniste,
    String arvo,
    Osallistumistieto osallistuminen,
    String tallettaja) {
  public Valintapiste withHakemusOid(String hakemusOid) {
    return new Valintapiste(hakemusOid, tunniste, arvo, osallistuminen, tallettaja);
  }

  public Valintapiste withArvo(String arvo) {
    return new Valintapiste(hakemusOid, tunniste, arvo, osallistuminen, tallettaja);
  }

  public Valintapiste withTunniste(String tunniste) {
    return new Valintapiste(hakemusOid, tunniste, arvo, osallistuminen, tallettaja);
  }

  public Pistetieto toPistetieto() {
    return new Pistetieto(tunniste, arvo, osallistuminen, tallettaja);
  }
}
