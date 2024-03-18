package fi.vm.sade.valintalaskenta.tulos.dao;

import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteerihistoria;
import java.util.List;
import java.util.UUID;

public interface TulosJarjestyskriteerihistoriaDAO {
  List<UUID> findByValintatapajonoAndHakemusOid(String valintatapajonoOid, String hakemusOid);

  List<Jarjestyskriteerihistoria> findByTunnisteet(List<UUID> tunnisteet);
}
