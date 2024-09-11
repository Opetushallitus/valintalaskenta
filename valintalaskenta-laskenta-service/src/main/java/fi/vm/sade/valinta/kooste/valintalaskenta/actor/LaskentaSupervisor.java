package fi.vm.sade.valinta.kooste.valintalaskenta.actor;

import fi.vm.sade.valinta.kooste.valintalaskenta.route.ValintalaskentaKerrallaRouteValvomo;

public interface LaskentaSupervisor extends ValintalaskentaKerrallaRouteValvomo {
  void ready(String uuid);

  void fetchAndStartLaskenta();
}
