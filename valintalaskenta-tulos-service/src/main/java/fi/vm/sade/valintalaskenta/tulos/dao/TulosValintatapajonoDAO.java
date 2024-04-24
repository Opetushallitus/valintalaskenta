package fi.vm.sade.valintalaskenta.tulos.dao;

import fi.vm.sade.valintalaskenta.domain.valinta.Valintatapajono;
import java.util.Optional;

public interface TulosValintatapajonoDAO {
  Optional<Valintatapajono> paivitaValmisSijoiteltavaksi(
      String valintatapajonoOid, boolean valmisSijoiteltavaksi);
}
