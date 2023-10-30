package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import fi.vm.sade.valintalaskenta.domain.valinta.Hakijaryhma;
import fi.vm.sade.valintalaskenta.tulos.dao.TulosHakijaryhmaDAO;
import fi.vm.sade.valintalaskenta.tulos.dao.repository.TulosHakijaryhmaRepository;
import java.util.Comparator;
import java.util.List;
import org.springframework.stereotype.Service;

@Service
public class TulosHakijaryhmaDAOImpl implements TulosHakijaryhmaDAO {

  private final TulosHakijaryhmaRepository repo;

  public TulosHakijaryhmaDAOImpl(TulosHakijaryhmaRepository repo) {
    this.repo = repo;
  }

  @Override
  public List<Hakijaryhma> readByHakukohdeOid(String hakukohdeoid) {
    return repo.findHakijaryhmasByHakukohdeOid(hakukohdeoid).stream()
        .sorted(Comparator.comparing(hakijaryhma -> hakijaryhma.prioriteetti))
        .toList();
  }
}
