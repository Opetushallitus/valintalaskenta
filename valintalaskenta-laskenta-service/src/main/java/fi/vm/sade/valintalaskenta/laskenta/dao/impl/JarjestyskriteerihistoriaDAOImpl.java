package fi.vm.sade.valintalaskenta.laskenta.dao.impl;

import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteerihistoria;
import fi.vm.sade.valintalaskenta.laskenta.dao.JarjestyskriteerihistoriaDAO;
import fi.vm.sade.valintalaskenta.tulos.dao.repository.JarjestyskriteerihistoriaRepository;
import java.util.List;
import java.util.UUID;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

@Service
public class JarjestyskriteerihistoriaDAOImpl implements JarjestyskriteerihistoriaDAO {

  private static final Logger LOG = LoggerFactory.getLogger(JarjestyskriteerihistoriaDAOImpl.class);

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
    /*Jarjestyskriteerihistoria historia = new Jarjestyskriteerihistoria();
    historia.setId(id);

    final ObjectEntity document = dokumenttipalvelu.get(id);
    try {
      historia.setHistoriaGzip(document.entity.readAllBytes());
    } catch (IOException e) {
      LOG.error("Unable to read document with id {}", id);
      throw new RuntimeException(e);
    }
    return JarjestyskriteeriKooderi.dekoodaa(historia);*/
  }

  @Override
  public List<Jarjestyskriteerihistoria> fetchOldest() {
    return repository.fetchOldest();
  }
}
