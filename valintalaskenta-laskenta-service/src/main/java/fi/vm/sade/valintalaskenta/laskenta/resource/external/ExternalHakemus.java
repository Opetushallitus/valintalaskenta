package fi.vm.sade.valintalaskenta.laskenta.resource.external;

import fi.vm.sade.valintalaskenta.domain.dto.valintapiste.Pistetieto;
import fi.vm.sade.valintalaskenta.domain.dto.valintapiste.PistetietoWrapper;
import java.util.Arrays;

public interface ExternalHakemus {
  String hakemusOid();

  String henkiloOid();

  default PistetietoWrapper toPistetietoWrapper(Pistetieto... pisteet) {
    return new PistetietoWrapper(hakemusOid(), henkiloOid(), Arrays.stream(pisteet).toList());
  }
}
