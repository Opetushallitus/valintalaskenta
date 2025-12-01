package fi.vm.sade.valintalaskenta.domain.dto.valintapiste;

import java.util.List;

public record PistetietoWrapper(String hakemusOID, String oppijaOID, List<Pistetieto> pisteet) {
  public PistetietoWrapper(String hakemusOID, List<Pistetieto> pisteet) {
    this(hakemusOID, null, pisteet);
  }

  public PistetietoWrapper(String hakemusOID) {
    this(hakemusOID, null, List.of());
  }

  public PistetietoWrapper withOppijaOID(String oppijaOID) {
    return new PistetietoWrapper(hakemusOID, oppijaOID, pisteet);
  }
}
