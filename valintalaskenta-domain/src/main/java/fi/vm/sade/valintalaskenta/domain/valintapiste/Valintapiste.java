package fi.vm.sade.valintalaskenta.domain.valintapiste;

import fi.vm.sade.valintalaskenta.domain.dto.valintapiste.Osallistumistieto;
import fi.vm.sade.valintalaskenta.domain.dto.valintapiste.ValintapisteDTO;
import jakarta.persistence.IdClass;

@IdClass(ValintapisteId.class)
public record Valintapiste(
    String hakemusOid,
    String tunniste,
    String arvo,
    Osallistumistieto osallistuminen,
    String tallettaja) {
  public Valintapiste withArvo(String arvo) {
    return new Valintapiste(hakemusOid, tunniste, arvo, osallistuminen, tallettaja);
  }

  public ValintapisteDTO toDTO() {
    return new ValintapisteDTO(hakemusOid, tunniste, arvo, osallistuminen, tallettaja);
  }
}
