package fi.vm.sade.valintalaskenta.laskenta.dao.impl;

import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteerihistoria;
import fi.vm.sade.valintalaskenta.laskenta.dao.JarjestyskriteerihistoriaDAO;
import fi.vm.sade.valintalaskenta.tulos.dao.repository.JarjestyskriteerihistoriaRepository;
import java.util.List;
import java.util.UUID;
import org.springframework.stereotype.Service;

@Service
public class JarjestyskriteerihistoriaDAOImpl implements JarjestyskriteerihistoriaDAO {

  private final JarjestyskriteerihistoriaRepository repository;

  public JarjestyskriteerihistoriaDAOImpl(
      JarjestyskriteerihistoriaRepository jarjestyskriteerihistoriaRepository) {
    this.repository = jarjestyskriteerihistoriaRepository;
  }

  @Override
  public void create(Jarjestyskriteerihistoria jarjestyskriteerihistoria) {
    if (jarjestyskriteerihistoria.getTunniste() == null) {
      jarjestyskriteerihistoria.setTunniste(UUID.randomUUID());
    }
    repository.save(jarjestyskriteerihistoria);
  }

  @Override
  public void createVersionWithUpdate(UUID tunniste) {
    Jarjestyskriteerihistoria historia = new Jarjestyskriteerihistoria();
    historia.setTunniste(tunniste);
    historia.setLaskettuUudelleen(true);
    repository.save(historia);
  }

  @Override
  public void delete(Long id) {
    repository.deleteById(id);
  }

  @Override
  public Jarjestyskriteerihistoria hae(UUID id) {
    return repository.findLatestByTunniste(id).orElse(null);
  }

  @Override
  public List<Jarjestyskriteerihistoria> fetchOldest() {
    return repository.fetchOldest();
  }
}
