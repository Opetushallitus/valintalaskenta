package fi.vm.sade.valintalaskenta.laskenta.dao.impl;

import fi.vm.sade.valintalaskenta.domain.valinta.*;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValinnanvaiheDAO;
import fi.vm.sade.valintalaskenta.laskenta.dao.repository.JonosijaRepository;
import fi.vm.sade.valintalaskenta.laskenta.dao.repository.ValinnanvaiheRepository;
import fi.vm.sade.valintalaskenta.tulos.dao.repository.TulosValintatapajonoRepository;
import java.util.Date;
import java.util.List;
import java.util.Optional;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
public class ValinnanvaiheDAOImpl implements ValinnanvaiheDAO {
  private static final Logger LOGGER = LoggerFactory.getLogger(ValinnanvaiheDAOImpl.class);

  private final ValinnanvaiheRepository repo;
  private final TulosValintatapajonoRepository vtpRepo;

  private final JonosijaRepository jonosijaRepo;

  public ValinnanvaiheDAOImpl(
      ValinnanvaiheRepository repo,
      TulosValintatapajonoRepository vtpRepo,
      JonosijaRepository jonosijaRepo) {
    this.vtpRepo = vtpRepo;
    this.repo = repo;
    this.jonosijaRepo = jonosijaRepo;
  }

  @Override
  public Valinnanvaihe haeEdeltavaValinnanvaihe(
      String hakuOid, String hakukohdeOid, int jarjestysnumero) {
    if (jarjestysnumero < 0) {
      return null;
    }
    return repo.findPreviousValinnanvaihe(hakuOid, hakukohdeOid, jarjestysnumero - 1).orElse(null);
  }

  @Override
  public Valinnanvaihe haeViimeisinValinnanvaihe(
      String hakuOid, String hakukohdeOid, int jarjestysnumero) {
    if (jarjestysnumero > 0) {
      return repo.findViimeisinEdeltavaValinnanvaihe(hakuOid, hakukohdeOid, jarjestysnumero)
          .orElse(null);
    }
    return null;
  }

  @Override
  public Valinnanvaihe haeValinnanvaihe(String valinnanvaiheOid) {
    return repo.findValinnanvaiheByValinnanvaiheOid(valinnanvaiheOid).orElse(null);
  }

  @Override
  public Optional<ValinnanvaiheLite> haeValinnanvaiheLite(String valinnanvaiheOid) {
    return repo.findValinnanvaiheLiteByValinnanvaiheOid(valinnanvaiheOid);
  }

  // @TODO: necessary anymore?
  @Override
  public List<Valinnanvaihe> haeValinnanvaiheetJarjestysnumerolla(
      String hakuOid, String hakukohdeOid, int jarjestysnumero) {
    return repo.findDistinctValinnanvaiheetJarjestysnumerolla(
        hakuOid, hakukohdeOid, jarjestysnumero);
  }

  @Transactional(propagation = Propagation.MANDATORY)
  @Override
  public void saveOrUpdate(Valinnanvaihe valinnanvaihe) {
    valinnanvaihe.setCreatedAt(new Date());
    repo.save(valinnanvaihe);
  }

  @Transactional
  @Override
  public void poistaValinnanvaihe(Valinnanvaihe valinnanvaihe) {
    valinnanvaihe.getValintatapajonot().forEach(this::poistaJono);
    repo.delete(valinnanvaihe);
  }

  @Transactional
  @Override
  public void poistaJono(Valintatapajono jono) {
    if (!jono.getJonosijat().isEmpty()) {
      jonosijaRepo.deleteAllById(jono.getJonosijat().stream().map(Jonosija::getId).toList());
    }
    vtpRepo.delete(jono);
  }
}
