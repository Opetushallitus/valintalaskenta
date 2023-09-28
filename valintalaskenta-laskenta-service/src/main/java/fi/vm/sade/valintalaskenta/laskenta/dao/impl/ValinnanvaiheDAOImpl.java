package fi.vm.sade.valintalaskenta.laskenta.dao.impl;

import fi.vm.sade.valintalaskenta.domain.valinta.*;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValinnanvaiheDAO;
import java.util.List;
import java.util.Optional;

import fi.vm.sade.valintalaskenta.laskenta.dao.repository.ValinnanvaiheRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Repository;

@Repository("valinnanvaiheDAO")
public class ValinnanvaiheDAOImpl implements ValinnanvaiheDAO {
  private static final Logger LOGGER = LoggerFactory.getLogger(ValinnanvaiheDAOImpl.class);

  private final ValinnanvaiheRepository repo;

  public ValinnanvaiheDAOImpl(ValinnanvaiheRepository repo) {
    this.repo = repo;
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

  @Override
  public List<Valinnanvaihe> haeValinnanvaiheetJarjestysnumerolla(
      String hakuOid, String hakukohdeOid, int jarjestysnumero) {
    return null;
    /*    return migrate(
    datastore
        .find(ValinnanvaiheMigrationDTO.class)
        .field("hakuOid")
        .equal(hakuOid)
        .field("hakukohdeOid")
        .equal(hakukohdeOid)
        .field("jarjestysnumero")
        .equal(jarjestysnumero)
        .asList());*/
  }

  @Override
  public void saveOrUpdate(Valinnanvaihe valinnanvaihe) {
    repo.save(valinnanvaihe);
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

  @Override
  public void poistaValinnanvaihe(Valinnanvaihe valinnanvaihe) {
    valinnanvaihe.getValintatapajono().forEach(this::poistaJono);
    // datastore.delete(valinnanvaihe);
  }

  @Override
  public void poistaJono(Valintatapajono jono) {
    /*    List<ObjectId> jonosijaIdt = jono.getJonosijaIdt();
    if (!jonosijaIdt.isEmpty()) {
      datastore.delete(datastore.createQuery(Jonosija.class).field("_id").in(jonosijaIdt));
    }
    datastore.delete(jono);*/
  }

  // TODO tämä ei vielä oikeasti poista mitään, logittaa vain poistettavien jonosijojen id:t.
  // (BUG1899)
  @Override
  public void poistaJononJonosijatHakemusOideilla(
      Valintatapajono jono, List<String> hakemusOidsToRemove) {
    /*    if (!hakemusOidsToRemove.isEmpty()) {
      LOGGER.warn("Poistetaan jonosijoja hakemuksille {}", hakemusOidsToRemove);
      List<ObjectId> tamanJononJonosijaIdt = jono.getJonosijaIdt();
      List<Jonosija> poistettavatJonosijat =
          datastore
              .createQuery(Jonosija.class)
              .field("_id")
              .in(tamanJononJonosijaIdt)
              .field("hakemusOid")
              .in(hakemusOidsToRemove)
              .asList();

      for (Jonosija j : poistettavatJonosijat) {
        LOGGER.warn(
            "Ollaan valmiita poistamaan hakemuksen {} jonosija ObjectId:llä {} ",
            j.getHakemusOid(),
            j.getId());
      }
    }*/
  }

  private void saveJonosijat(Valintatapajono valintatapajono) {
    /*    valintatapajono.setJonosijaIdt(
    valintatapajono.getJonosijat().stream()
        .map(jonosija -> (ObjectId) datastore.save(jonosija).getId())
        .collect(Collectors.toList()));*/
  }

  private void populateJonosijat(Valintatapajono valintatapajono) {
    /*    List<ObjectId> jonosijaIdt = valintatapajono.getJonosijaIdt();
    if (jonosijaIdt.isEmpty()) {
      valintatapajono.setJonosijat(new ArrayList<>());
    } else {
      valintatapajono.setJonosijat(
          datastore.createQuery(Jonosija.class).field("_id").in(jonosijaIdt).asList());
    }*/
  }
}
