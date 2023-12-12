package fi.vm.sade.valintalaskenta.tulos.service.impl;

import fi.vm.sade.valinta.dokumenttipalvelu.Dokumenttipalvelu;
import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteerihistoria;
import fi.vm.sade.valintalaskenta.tulos.dao.TulosJarjestyskriteerihistoriaDAO;
import fi.vm.sade.valintalaskenta.tulos.service.JarjestyskriteerihistoriaService;
import java.util.List;
import java.util.UUID;
import org.springframework.stereotype.Service;

@Service
public class JarjestyskriteerihistoriaServiceImpl implements JarjestyskriteerihistoriaService {

  private final Dokumenttipalvelu dokumenttipalvelu;

  private final TulosJarjestyskriteerihistoriaDAO historiaDAO;

  public JarjestyskriteerihistoriaServiceImpl(
      TulosJarjestyskriteerihistoriaDAO historiaDAO, Dokumenttipalvelu dokumenttipalvelu) {
    this.historiaDAO = historiaDAO;
    this.dokumenttipalvelu = dokumenttipalvelu;
  }

  @Override
  public List<Jarjestyskriteerihistoria> findByValintatapajonoAndHakemusOid(
      String valintatapajonoOid, String hakemusOid) {
    List<UUID> historyIds =
        historiaDAO.findByValintatapajonoAndHakemusOid(valintatapajonoOid, hakemusOid);
    return null;
  }
}
