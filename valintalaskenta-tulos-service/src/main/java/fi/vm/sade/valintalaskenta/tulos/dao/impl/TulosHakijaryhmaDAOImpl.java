package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import fi.vm.sade.valintalaskenta.domain.valinta.Hakijaryhma;
import fi.vm.sade.valintalaskenta.tulos.dao.TulosHakijaryhmaDAO;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

import fi.vm.sade.valintalaskenta.tulos.dao.repository.TulosHakijaryhmaRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Repository;

@Repository
public class TulosHakijaryhmaDAOImpl implements TulosHakijaryhmaDAO {
  private static final Logger LOGGER = LoggerFactory.getLogger(TulosHakijaryhmaDAOImpl.class);

  private final TulosHakijaryhmaRepository repo;

  public TulosHakijaryhmaDAOImpl(TulosHakijaryhmaRepository repo) {
    this.repo = repo;
  }


  @Override
  public List<Hakijaryhma> readByHakukohdeOid(String hakukohdeoid) {
    return repo.findHakijaryhmasByHakukohdeOid(hakukohdeoid)
      .stream()
      .sorted(Comparator.comparing(hakijaryhma -> hakijaryhma.prioriteetti))
      .toList();
  }
}
