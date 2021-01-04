package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import com.mongodb.BasicDBObject;
import com.mongodb.BasicDBObjectBuilder;
import com.mongodb.DBObject;
import fi.vm.sade.valintalaskenta.domain.valinta.*;
import fi.vm.sade.valintalaskenta.tulos.dao.ValinnanvaiheDAO;
import fi.vm.sade.valintalaskenta.tulos.logging.LaskentaAuditLog;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;
import org.apache.commons.lang3.tuple.Pair;
import org.bson.types.ObjectId;
import org.mongodb.morphia.AdvancedDatastore;
import org.mongodb.morphia.Datastore;
import org.mongodb.morphia.Key;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Repository;

@Repository
public class ValinnanvaiheDAOImpl implements ValinnanvaiheDAO {
  private static final Logger LOGGER = LoggerFactory.getLogger(ValinnanvaiheDAOImpl.class);

  @Qualifier("datastore2")
  @Autowired
  private Datastore datastore;

  @Autowired private LaskentaAuditLog auditLog;

  @Override
  public List<Valinnanvaihe> readByHakuOidAndHakemusOid(String hakuOid, String hakemusOid) {
    List<ObjectId> hakemuksenJonosijaIdt = new LinkedList<>();
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
    return migrate(valinnanvaiheet);
  }

  @Override
  public Valinnanvaihe findByValintatapajonoOid(String valintatapajonoOid) {
    List<Key<Valintatapajono>> keys =
        datastore
            .find(Valintatapajono.class)
            .field("valintatapajonoOid")
            .equal(valintatapajonoOid)
            .asKeyList();
    if (keys.isEmpty()) {
      return null;
    }
    return migrate(
        datastore
            .createQuery(ValinnanvaiheMigrationDTO.class)
            .field("valintatapajonot")
            .in(keys)
            .get());
  }

  @Override
  public List<Valinnanvaihe> readByHakukohdeOid(String hakukohdeoid) {
    return migrate(
        datastore
            .createQuery(ValinnanvaiheMigrationDTO.class)
            .field("hakukohdeOid")
            .equal(hakukohdeoid)
            .asList());
  }

  @Override
  public List<Valinnanvaihe> readByHakuOid(String hakuoid) {
    return migrate(
        datastore
            .createQuery(ValinnanvaiheMigrationDTO.class)
            .field("hakuOid")
            .equal(hakuoid)
            .asList());
  }

  @Override
  public Stream<Valinnanvaihe> readByHakuOidStreaming(String hakuoid) {
    final Iterator<ValinnanvaiheMigrationDTO> mongoResultsIterator =
        datastore
            .createQuery(ValinnanvaiheMigrationDTO.class)
            .field("hakuOid")
            .equal(hakuoid)
            .iterator();

    return StreamSupport.stream(
            Spliterators.spliteratorUnknownSize(mongoResultsIterator, Spliterator.ORDERED), false)
        .map(this::migrate);
  }

  @Override
  public List<ValintatapajonoMigrationDTO> valintatapajonotJotkaEivatKaytaLaskentaa() {
    return datastore
        .createQuery(ValintatapajonoMigrationDTO.class)
        .field("kaytetaanValintalaskentaa")
        .notEqual(true)
        .retrievedFields(true, "_id")
        .asList();
  }

  @Override
  public List<Pair<String, String>> hakuOidHakukohdeOidPairsForJonos(
      List<ValintatapajonoMigrationDTO> validValintatapajonos) {
    return datastore
        .createQuery(ValinnanvaiheMigrationDTO.class)
        .field("valintatapajonot")
        .in(validValintatapajonos)
        .retrievedFields(true, "hakukohdeOid", "hakuOid")
        .asList()
        .stream()
        .map(vv -> Pair.of(vv.getHakuOid(), vv.getHakukohdeOid()))
        .distinct()
        .collect(Collectors.toList());
  }

  @Override
  public Valinnanvaihe haeValinnanvaihe(String valinnanvaiheOid) {
    return Optional.ofNullable(
            datastore
                .find(ValinnanvaiheMigrationDTO.class)
                .field("valinnanvaiheOid")
                .equal(valinnanvaiheOid)
                .get())
        .map(vaihe -> migrate(vaihe))
        .orElse(null);
  }

  @Override
  public void saveOrUpdate(Valinnanvaihe vaihe) {
    vaihe
        .getValintatapajonot()
        .forEach(
            valintatapajono -> {
              saveJonosijat(valintatapajono);
              datastore.save(valintatapajono);
            });
    datastore.save(vaihe);
  }

  private void saveJonosijat(Valintatapajono valintatapajono) {
    valintatapajono.setJonosijaIdt(
        valintatapajono.getJonosijat().stream()
            .map(jonosija -> (ObjectId) datastore.save(jonosija).getId())
            .collect(Collectors.toList()));
  }

  private void populateJonosijat(Valintatapajono valintatapajono) {
    List<ObjectId> jonosijaIdt = valintatapajono.getJonosijaIdt();
    if (jonosijaIdt.isEmpty()) {
      valintatapajono.setJonosijat(new ArrayList<>());
    } else {
      valintatapajono.setJonosijat(
          datastore.createQuery(Jonosija.class).field("_id").in(jonosijaIdt).asList());
    }
  }

  private Valintatapajono migrate(ValintatapajonoMigrationDTO jono) {
    if (jono.getSchemaVersion() == Valintatapajono.CURRENT_SCHEMA_VERSION) {
      Valintatapajono alreadyMigrated =
          datastore.find(Valintatapajono.class).field("_id").equal(jono.getId()).get();
      populateJonosijat(alreadyMigrated);
      return alreadyMigrated;
    } else {
      LOGGER.info("Migrating valintatapajono {}", jono.getValintatapajonoOid());
      Valintatapajono migratedJono = jono.migrate();
      saveJonosijat(migratedJono);
      saveJono(migratedJono);
      return migratedJono;
    }
  }

  private Valinnanvaihe migrate(ValinnanvaiheMigrationDTO vaihe) {
    Valinnanvaihe migrated = new Valinnanvaihe();
    migrated.setId(vaihe.getId());
    migrated.setJarjestysnumero(vaihe.getJarjestysnumero());
    migrated.setCreatedAt(vaihe.getCreatedAt());
    migrated.setHakuOid(vaihe.getHakuOid());
    migrated.setHakukohdeOid(vaihe.getHakukohdeOid());
    migrated.setValinnanvaiheOid(vaihe.getValinnanvaiheOid());
    migrated.setTarjoajaOid(vaihe.getTarjoajaOid());
    migrated.setNimi(vaihe.getNimi());
    migrated.setValintatapajonot(
        vaihe.getValintatapajonot().stream()
            .map(
                jono -> {
                  try {
                    return migrate(jono);
                  } catch (RuntimeException e) {
                    LOGGER.error(
                        String.format(
                            "Error when calling migrate for jono %s of valinnanvaihe %s",
                            jono.getId(), vaihe.getId()),
                        e);
                    throw e;
                  }
                })
            .collect(Collectors.toList()));
    return migrated;
  }

  private List<Valinnanvaihe> migrate(List<ValinnanvaiheMigrationDTO> vaiheet) {
    return vaiheet.stream().map(vaihe -> migrate(vaihe)).collect(Collectors.toList());
  }

  private Key<Valintatapajono> saveJono(Valintatapajono valintatapajono) {
    return datastore.save(valintatapajono);
  }

  @Override
  public Key<Valinnanvaihe> saveVaihe(Valinnanvaihe vaihe) {
    return datastore.save(vaihe);
  }

  private Key<Jonosija> saveJonosija(Jonosija jonosija) {
    return datastore.save(jonosija);
  }
}
