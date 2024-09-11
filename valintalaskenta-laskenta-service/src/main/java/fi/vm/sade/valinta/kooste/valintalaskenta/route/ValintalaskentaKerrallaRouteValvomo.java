package fi.vm.sade.valinta.kooste.valintalaskenta.route;

import fi.vm.sade.valinta.kooste.valintalaskenta.dto.Laskenta;
import java.util.List;
import java.util.Optional;

public interface ValintalaskentaKerrallaRouteValvomo {
  Optional<Laskenta> fetchLaskenta(String uuid);

  List<Laskenta> runningLaskentas();
}
