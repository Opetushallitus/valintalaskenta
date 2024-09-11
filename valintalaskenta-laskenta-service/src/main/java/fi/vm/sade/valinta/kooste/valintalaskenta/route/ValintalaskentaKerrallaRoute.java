package fi.vm.sade.valinta.kooste.valintalaskenta.route;

import fi.vm.sade.valinta.kooste.external.resource.ohjausparametrit.dto.ParametritDTO;
import fi.vm.sade.valinta.kooste.external.resource.tarjonta.Haku;
import fi.vm.sade.valinta.kooste.valintalaskenta.dto.LaskentaStartParams;

public interface ValintalaskentaKerrallaRoute {
  void suoritaValintalaskentaKerralla(
      final Haku haku, final ParametritDTO parametritDTO, LaskentaStartParams laskentaStartParams);

  void workAvailable();
}
