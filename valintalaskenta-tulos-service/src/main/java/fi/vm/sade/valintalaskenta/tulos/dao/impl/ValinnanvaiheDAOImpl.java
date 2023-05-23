package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import static dev.morphia.query.filters.Filters.*;

import dev.morphia.Datastore;
import dev.morphia.query.FindOptions;
import fi.vm.sade.valintalaskenta.domain.valinta.*;
import fi.vm.sade.valintalaskenta.tulos.dao.ValinnanvaiheDAO;
import fi.vm.sade.valintalaskenta.tulos.logging.LaskentaAuditLog;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;
import org.apache.commons.lang3.tuple.Pair;
import org.bson.types.ObjectId;
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
    datastore.find(Jonosija.class).filter(eq("hakemusOid", hakemusOid)).stream()
        .forEach(key -> hakemuksenJonosijaIdt.add(key.getId()));
    if (hakemuksenJonosijaIdt.isEmpty()) {
      return new ArrayList<>();
    }
    List<ObjectId> hakemuksenValintatapajonoIdt = new LinkedList<>();
    datastore.find(Valintatapajono.class).filter(in("jonosijaIdt", hakemuksenJonosijaIdt)).stream()
        .forEach(key -> hakemuksenValintatapajonoIdt.add(key.getId()));
    if (hakemuksenValintatapajonoIdt.isEmpty()) {
      return new ArrayList<>();
    }
    List<ValinnanvaiheMigrationDTO> valinnanvaiheet =
        datastore
            .find(ValinnanvaiheMigrationDTO.class)
            .filter(
                and(
                    eq("hakuOid", hakuOid),
                    in("valintatapajonot.id", hakemuksenValintatapajonoIdt)))
            .stream()
            .collect(Collectors.toList());
    return migrate(valinnanvaiheet);
  }

  @Override
  public Valinnanvaihe findByValintatapajonoOid(String valintatapajonoOid) {
    List<ObjectId> valintatapajonoIdt = new LinkedList<>();
    datastore
        .find(Valintatapajono.class)
        .filter(eq("valintatapajonoOid", valintatapajonoOid))
        .stream()
        .forEach(key -> valintatapajonoIdt.add(key.getId()));
    if (valintatapajonoIdt.isEmpty()) {
      return null;
    }
    ValinnanvaiheMigrationDTO valinnanvaihe =
        datastore
            .find(ValinnanvaiheMigrationDTO.class)
            .filter(in("valintatapajonot.id", valintatapajonoIdt))
            .first();
    return migrate(valinnanvaihe);
  }

  @Override
  public List<Valinnanvaihe> readByHakukohdeOid(String hakukohdeoid) {
    return migrate(
        datastore
            .find(ValinnanvaiheMigrationDTO.class)
            .filter(eq("hakukohdeOid", hakukohdeoid))
            .stream()
            .collect(Collectors.toList()));
  }

  @Override
  public List<Valinnanvaihe> readByHakuOid(String hakuoid) {
    return migrate(
        datastore.find(ValinnanvaiheMigrationDTO.class).filter(eq("hakuOid", hakuoid)).stream()
            .collect(Collectors.toList()));
  }

  @Override
  public Stream<Valinnanvaihe> readByHakuOidStreaming(String hakuoid) {
    final Iterator<ValinnanvaiheMigrationDTO> mongoResultsIterator =
        datastore.find(ValinnanvaiheMigrationDTO.class).filter(eq("hakuOid", hakuoid)).iterator();

    return StreamSupport.stream(
            Spliterators.spliteratorUnknownSize(mongoResultsIterator, Spliterator.ORDERED), false)
        .map(this::migrate);
  }

  @Override
  public List<ValintatapajonoMigrationDTO> valintatapajonotJotkaEivatKaytaLaskentaa() {
    return datastore
        .find(ValintatapajonoMigrationDTO.class)
        .filter(ne("kaytetaanValintalaskentaa", true))
        .iterator(new FindOptions().projection().include("_id"))
        .toList();
  }

  @Override
  public List<Pair<String, String>> hakuOidHakukohdeOidPairsForJonos(
      List<ValintatapajonoMigrationDTO> validValintatapajonos) {
    return datastore
        .find(ValinnanvaiheMigrationDTO.class)
        .filter(in("valintatapajonot", validValintatapajonos))
        .iterator(new FindOptions().projection().include("hakukohdeOid", "hakuOid"))
        .toList()
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
                .filter(eq("valinnanvaiheOid", valinnanvaiheOid))
                .first())
        .map(this::migrate)
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
    List<ObjectId> jonosijaIdt =
        valintatapajono != null ? valintatapajono.getJonosijaIdt() : new ArrayList<>();
    if (jonosijaIdt.isEmpty()) {
      valintatapajono.setJonosijat(new ArrayList<>());
    } else {
      valintatapajono.setJonosijat(
          datastore.find(Jonosija.class).filter(in("_id", jonosijaIdt)).stream()
              .collect(Collectors.toList()));
    }
  }

  private Valintatapajono migrate(ValintatapajonoMigrationDTO jono) {
    if (jono.getSchemaVersion() == Valintatapajono.CURRENT_SCHEMA_VERSION) {
      Valintatapajono alreadyMigrated =
          datastore.find(Valintatapajono.class).filter(eq("_id", jono.getId())).first();
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

  private Valintatapajono saveJono(Valintatapajono valintatapajono) {
    return datastore.save(valintatapajono);
  }

  @Override
  public Valinnanvaihe saveVaihe(Valinnanvaihe vaihe) {
    return datastore.save(vaihe);
  }

  private Jonosija saveJonosija(Jonosija jonosija) {
    return datastore.save(jonosija);
  }
}
