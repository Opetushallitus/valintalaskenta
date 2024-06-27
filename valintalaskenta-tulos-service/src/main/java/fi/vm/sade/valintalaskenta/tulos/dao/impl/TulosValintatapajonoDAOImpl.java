package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import fi.vm.sade.valintalaskenta.domain.valinta.Valintatapajono;
import fi.vm.sade.valintalaskenta.tulos.dao.TulosValintatapajonoDAO;
import fi.vm.sade.valintalaskenta.tulos.dao.repository.TulosValintatapajonoRepository;
import java.util.Optional;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class TulosValintatapajonoDAOImpl implements TulosValintatapajonoDAO {
  private static final Logger LOGGER = LoggerFactory.getLogger(TulosValintatapajonoDAOImpl.class);

  private final TulosValintatapajonoRepository tulosValintatapajonoRepository;

  public TulosValintatapajonoDAOImpl(
      TulosValintatapajonoRepository tulosValintatapajonoRepository) {
    this.tulosValintatapajonoRepository = tulosValintatapajonoRepository;
  }

  @Override
  @Transactional
  public Optional<Valintatapajono> paivitaValmisSijoiteltavaksi(
      String valintatapajonoOid, boolean valmisSijoiteltavaksi) {
    return tulosValintatapajonoRepository.paivitaValmisSijoiteltavaksi(
        valintatapajonoOid, valmisSijoiteltavaksi);
  }
}
