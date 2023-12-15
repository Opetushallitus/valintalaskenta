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
    repository.save(jarjestyskriteerihistoria);
  }

  @Override
  public void delete(UUID id) {
    repository.deleteById(id);
  }

  @Override
  public Jarjestyskriteerihistoria hae(UUID id) {
    return repository.findById(id).orElse(null);
  }

  @Override
  public List<Jarjestyskriteerihistoria> fetchOldest() {
    return repository.fetchOldest();
  }
}
