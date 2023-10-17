package fi.vm.sade.valintalaskenta.laskenta.dao.impl;

import fi.vm.sade.valintalaskenta.domain.valinta.*;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValinnanvaiheDAO;
import java.util.List;
import java.util.Optional;

import fi.vm.sade.valintalaskenta.laskenta.dao.repository.JonosijaRepository;
import fi.vm.sade.valintalaskenta.laskenta.dao.repository.ValinnanvaiheRepository;
import fi.vm.sade.valintalaskenta.laskenta.dao.repository.ValintatapajonoRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

@Repository("valinnanvaiheDAO")
public class ValinnanvaiheDAOImpl implements ValinnanvaiheDAO {
  private static final Logger LOGGER = LoggerFactory.getLogger(ValinnanvaiheDAOImpl.class);

  private final ValinnanvaiheRepository repo;
  private final ValintatapajonoRepository vtpRepo;

  private final JonosijaRepository jonosijaRepo;

  public ValinnanvaiheDAOImpl(ValinnanvaiheRepository repo, ValintatapajonoRepository vtpRepo, JonosijaRepository jonosijaRepo) {
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
      return repo.findViimeisinEdeltavaValinnanvaihe(hakuOid, hakukohdeOid, jarjestysnumero).orElse(null);
    }
    return null;
    /*    if (jarjestysnumero > 0) {
      return Optional.ofNullable(
              datastore
                  .find(ValinnanvaiheMigrationDTO.class)
                  .field("hakuOid")
                  .equal(hakuOid)
                  .field("hakukohdeOid")
                  .equal(hakukohdeOid)
                  .field("jarjestysnumero")
                  .lessThan(jarjestysnumero)
                  .order("-jarjestysnumero")
                  .limit(1)
                  .get())
          .map(this::migrate)
          .orElse(null);
    } else {
      return null;
    }*/
  }

  @Override
  public Valinnanvaihe haeValinnanvaihe(String valinnanvaiheOid) {
    return repo.findValinnanvaiheByValinnanvaiheOid(valinnanvaiheOid).orElse(null);
    /*    return Optional.ofNullable(
        datastore
            .find(ValinnanvaiheMigrationDTO.class)
            .field("valinnanvaiheOid")
            .equal(valinnanvaiheOid)
            .get())
    .map(this::migrate)
    .orElse(null);*/
  }

  @Override
  public Optional<ValinnanvaiheLite> haeValinnanvaiheLite(String valinnanvaiheOid) {
    return repo.findValinnanvaiheLiteByValinnanvaiheOid(valinnanvaiheOid);
  }

  //@TODO: necessary anymore?
  @Override
  public List<Valinnanvaihe> haeValinnanvaiheetJarjestysnumerolla(
      String hakuOid, String hakukohdeOid, int jarjestysnumero) {
    return repo.findDistinctValinnanvaiheetJarjestysnumerolla(hakuOid, hakukohdeOid, jarjestysnumero);
  }

  @Override
  public void saveOrUpdate(Valinnanvaihe valinnanvaihe) {
    repo.save(valinnanvaihe);
    //TODO: remove?
    /*    valinnanvaihe.reportDuplicateValintatapajonoOids();
    valinnanvaihe
        .getValintatapajonot()
        .forEach(
            valintatapajono -> {
              saveJonosijat(valintatapajono);
              datastore.save(valintatapajono);
            });
    datastore.save(valinnanvaihe);*/
  }

  @Transactional
  @Override
  public void poistaValinnanvaihe(Valinnanvaihe valinnanvaihe) {
    valinnanvaihe.getValintatapajono().forEach(this::poistaJono);
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
