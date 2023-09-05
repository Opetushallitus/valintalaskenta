package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import fi.vm.sade.valintalaskenta.domain.valinta.*;
import fi.vm.sade.valintalaskenta.tulos.dao.ValinnanvaiheDAO;
import fi.vm.sade.valintalaskenta.tulos.logging.LaskentaAuditLog;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;
import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Repository;

@Repository
public class ValinnanvaiheDAOImpl implements ValinnanvaiheDAO {
  private static final Logger LOGGER = LoggerFactory.getLogger(ValinnanvaiheDAOImpl.class);


  @Qualifier("LaskentaAuditLog")
  @Autowired private LaskentaAuditLog auditLog;

  @Override
  public List<Valinnanvaihe> readByHakukohdeOid(String hakukohdeoid) {
    return null;
  }

  @Override
  public List<Valinnanvaihe> readByHakuOid(String hakuoid) {
    return null;
  }

  @Override
  public Stream<Valinnanvaihe> readByHakuOidStreaming(String hakuoid) {
    return null;
  }

  @Override
  public List<Valinnanvaihe> readByHakuOidAndHakemusOid(String hakuOid, String hakemusOid) {
    return null;
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
    return null;
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
    return null;
  }

  @Override
  public void saveOrUpdate(Valinnanvaihe vaihe) {
    /*vaihe
        .getValintatapajonot()
        .forEach(
            valintatapajono -> {
              saveJonosijat(valintatapajono);
              datastore.save(valintatapajono);
            });
    datastore.save(vaihe);*/
  }

  private void saveJonosijat(Valintatapajono valintatapajono) {
/*    valintatapajono.setJonosijaIdt(
        valintatapajono.getJonosijat().stream()
            .map(jonosija -> (ObjectId) datastore.save(jonosija).getId())
            .collect(Collectors.toList()));*/
  }

  private String saveJono(Valintatapajono valintatapajono) {
    return valintatapajono.getId(); //datastore.save(valintatapajono);
  }

  @Override
  public String saveVaihe(Valinnanvaihe vaihe) {
    return vaihe.getId(); // datastore.save(vaihe);
  }

  private String saveJonosija(Jonosija jonosija) {
    return jonosija.getId(); // datastore.save(jonosija);
  }
}
