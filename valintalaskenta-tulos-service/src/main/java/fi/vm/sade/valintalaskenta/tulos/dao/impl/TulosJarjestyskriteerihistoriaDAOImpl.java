package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import fi.vm.sade.valintalaskenta.domain.valinta.*;
import fi.vm.sade.valintalaskenta.tulos.dao.TulosJarjestyskriteerihistoriaDAO;
import fi.vm.sade.valintalaskenta.tulos.dao.repository.JarjestyskriteerihistoriaRepository;
import fi.vm.sade.valintalaskenta.tulos.dao.repository.ValintatapajonoRepository;
import java.util.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Repository;

@Repository("jonosijaHistoriaTulosDAO")
public class TulosJarjestyskriteerihistoriaDAOImpl implements TulosJarjestyskriteerihistoriaDAO {
  private static final Logger LOG =
      LoggerFactory.getLogger(TulosJarjestyskriteerihistoriaDAOImpl.class);

  private final JarjestyskriteerihistoriaRepository repo;
  private final ValintatapajonoRepository vtpRepo;

  public TulosJarjestyskriteerihistoriaDAOImpl(
      JarjestyskriteerihistoriaRepository jarjestyskriteerihistoriaRepository,
      ValintatapajonoRepository valintatapajonoRepository) {
    this.repo = jarjestyskriteerihistoriaRepository;
    this.vtpRepo = valintatapajonoRepository;
  }

  @Override
  public List<UUID> findByValintatapajonoAndHakemusOid(
      String valintatapajonoOid, String hakemusOid) {
    return vtpRepo.findValintatapajonoByValintatapajonoOid(valintatapajonoOid).stream()
        .flatMap(jono -> jono.getJonosijat().stream())
        .filter(js -> hakemusOid.equals(js.getHakemusOid()))
        .flatMap(js -> js.getJarjestyskriteeritulokset().getJarjestyskriteeritulokset().stream())
        .sorted(Comparator.comparingInt(Jarjestyskriteeritulos::getPrioriteetti))
        .map(Jarjestyskriteeritulos::getHistoria)
        .toList();
  }

  @Override
  public List<Jarjestyskriteerihistoria> findByTunnisteet(List<UUID> tunnisteet) {
    return repo.findLatestByTunnisteet(tunnisteet).stream()
        .sorted(Comparator.comparingInt(historia -> tunnisteet.indexOf(historia.getTunniste())))
        .toList();
  }
}
