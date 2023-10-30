package fi.vm.sade.valintalaskenta.tulos.dao;

import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteerihistoria;
import java.util.List;

// TODO Change or delete in OK-384
public interface JarjestyskriteerihistoriaDAO {
  List<Jarjestyskriteerihistoria> findByValintatapajonoAndHakemusOid(
      String valintatapajonoOid, String hakemusOid);
}
