package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import fi.vm.sade.valintalaskenta.domain.valinta.*;
import fi.vm.sade.valintalaskenta.tulos.dao.TulosValinnanvaiheDAO;

import java.util.*;
import java.util.stream.Stream;

import fi.vm.sade.valintalaskenta.tulos.dao.repository.TulosValinnanvaiheRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Repository;

@Repository
public class TulosValinnanvaiheDAOImpl implements TulosValinnanvaiheDAO {
  private static final Logger LOGGER = LoggerFactory.getLogger(TulosValinnanvaiheDAOImpl.class);

  private final TulosValinnanvaiheRepository repo;

  public TulosValinnanvaiheDAOImpl(TulosValinnanvaiheRepository repo) {
    this.repo = repo;
  }

  @Override
  public List<Valinnanvaihe> readByHakukohdeOid(String hakukohdeoid) {
    return repo.findDistinctValinnanvaihesByHakukohdeOid(hakukohdeoid);
  }

  @Override
  public List<Valinnanvaihe> readByHakuOid(String hakuoid) {
    return repo.findDistinctValinnanvaihesByHakuOid(hakuoid).toList();
  }

  @Override
  public Stream<Valinnanvaihe> readByHakuOidStreaming(String hakuoid) {
    return repo.findDistinctValinnanvaihesByHakuOid(hakuoid);
  }

  @Override
  public List<Valinnanvaihe> readByHakuOidAndHakemusOid(String hakuOid, String hakemusOid) {
    return repo.findDistinctValinnanvaihesByHakuOidAndHakemusOid(hakuOid, hakemusOid);
/*    List<ObjectId> hakemuksenJonosijaIdt = new LinkedList<>();
    datastore
        .find(Jonosija.class)
        .field("hakemusOid")
        .equal(hakemusOid)
        .fetchKeys()
        .forEach(key -> hakemuksenJonosijaIdt.add((ObjectId) key.getId()));
    if (hakemuksenJonosijaIdt.isEmpty()) {
      return new ArrayList<>();
    }
    List<ObjectId> hakemuksenValintatapajonoIdt = new LinkedList<>();
    datastore
        .find(Valintatapajono.class)
        .field("jonosijaIdt")
        .in(hakemuksenJonosijaIdt)
        .fetchKeys()
        .forEach(key -> hakemuksenValintatapajonoIdt.add((ObjectId) key.getId()));
    if (hakemuksenValintatapajonoIdt.isEmpty()) {
      return new ArrayList<>();
    }
    DBObject valinnanvaiheQuery =
        BasicDBObjectBuilder.start()
            .add("hakuOid", hakuOid)
            .add("valintatapajonot.$id", new BasicDBObject("$in", hakemuksenValintatapajonoIdt))
            .get();
    List<ValinnanvaiheMigrationDTO> valinnanvaiheet =
        ((AdvancedDatastore) datastore)
            .createQuery(ValinnanvaiheMigrationDTO.class, valinnanvaiheQuery)
            .asList();
    return migrate(valinnanvaiheet);*/
  }

  @Override
  public Valinnanvaihe findByValintatapajonoOid(String valintatapajonoOid) {
    return repo.findValinnanvaiheByValintatapajono(valintatapajonoOid).orElse(null);
/*    List<ObjectId> valintatapajonoIdt = new LinkedList<>();
    datastore
        .find(Valintatapajono.class)
        .field("valintatapajonoOid")
        .equal(valintatapajonoOid)
        .fetchKeys()
        .forEach(key -> valintatapajonoIdt.add((ObjectId) key.getId()));
    if (valintatapajonoIdt.isEmpty()) {
      return null;
    }
    DBObject valinnanvaiheQuery =
        BasicDBObjectBuilder.start()
            .add("valintatapajonot.$id", new BasicDBObject("$in", valintatapajonoIdt))
            .get();
    ValinnanvaiheMigrationDTO valinnanvaihe =
        ((AdvancedDatastore) datastore)
            .createQuery(ValinnanvaiheMigrationDTO.class, valinnanvaiheQuery)
            .get();
    return migrate(valinnanvaihe);*/
  }

  @Override
  public Valinnanvaihe haeValinnanvaihe(String valinnanvaiheOid) {
    return repo.findValinnanvaiheByValinnanvaiheOid(valinnanvaiheOid).orElse(null);
  }

  @Override
  public void saveOrUpdate(Valinnanvaihe vaihe) {
    repo.save(vaihe);
    /*vaihe
        .getValintatapajonot()
        .forEach(
            valintatapajono -> {
              saveJonosijat(valintatapajono);
              datastore.save(valintatapajono);
            });
    datastore.save(vaihe);*/
  }

  @Override
  public UUID saveVaihe(Valinnanvaihe vaihe) {
    return repo.save(vaihe).getId();
  }

}
