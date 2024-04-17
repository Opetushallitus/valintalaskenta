package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import fi.vm.sade.valintalaskenta.domain.valinta.MuokattuJonosija;
import fi.vm.sade.valintalaskenta.tulos.dao.MuokattuJonosijaDAO;
import fi.vm.sade.valintalaskenta.tulos.dao.repository.MuokattuJonosijaRepository;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

@Service
public class MuokattuJonosijaDAOImpl implements MuokattuJonosijaDAO {
  private static final Logger LOGGER = LoggerFactory.getLogger(MuokattuJonosijaDAOImpl.class);

  private final MuokattuJonosijaRepository repo;

  public MuokattuJonosijaDAOImpl(MuokattuJonosijaRepository repo) {
    this.repo = repo;
  }

  @Override
  public List<MuokattuJonosija> readByHakuOid(String hakuOid) {
    return repo.findMuokattuJonosijasByHakuOid(hakuOid);
  }

  @Override
  public List<MuokattuJonosija> readByhakukohdeOid(String hakukohdeOid) {
    return repo.findMuokattuJonosijasByHakukohdeOid(hakukohdeOid);
  }

  @Override
  public List<MuokattuJonosija> readByHakuOidAndHakemusOid(String hakuOid, String hakemusOid) {
    return repo.findMuokattuJonosijasByHakuOidAndHakemusOid(hakuOid, hakemusOid);
  }

  @Override
  public MuokattuJonosija readByValintatapajonoOid(String valintatapajonoOid, String hakemusOid) {
    return repo.findMuokattuJonosijaByValintatapajonoOidAndHakemusOid(
            valintatapajonoOid, hakemusOid)
        .orElse(null);
  }

  @Override
  public void saveOrUpdate(MuokattuJonosija muokattuJonosija) {
    repo.save(muokattuJonosija);
  }
}
