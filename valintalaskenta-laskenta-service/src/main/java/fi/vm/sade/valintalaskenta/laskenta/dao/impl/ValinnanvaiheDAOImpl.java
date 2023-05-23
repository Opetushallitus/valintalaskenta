package fi.vm.sade.valintalaskenta.laskenta.dao.impl;

import static dev.morphia.query.Sort.descending;
import static dev.morphia.query.filters.Filters.*;

import dev.morphia.Datastore;
import dev.morphia.query.FindOptions;
import fi.vm.sade.valintalaskenta.domain.valinta.*;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValinnanvaiheDAO;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import org.bson.types.ObjectId;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

@Repository("valinnanvaiheDAO")
public class ValinnanvaiheDAOImpl implements ValinnanvaiheDAO {
  private static final Logger LOGGER = LoggerFactory.getLogger(ValinnanvaiheDAOImpl.class);

  @Autowired private Datastore datastore;

  @Override
  public Valinnanvaihe haeEdeltavaValinnanvaihe(
      String hakuOid, String hakukohdeOid, int jarjestysnumero) {
    if (jarjestysnumero > 0) {
      return Optional.ofNullable(
              datastore
                  .find(ValinnanvaiheMigrationDTO.class)
                  .filter(
                      and(
                          eq("hakuOid", hakuOid),
                          eq("hakukohdeOid", hakukohdeOid),
                          eq("jarjestysnumero", jarjestysnumero - 1)))
                  .first())
          .map(this::migrate)
          .orElse(null);
    } else {
      return null;
    }
  }

  @Override
  public Valinnanvaihe haeViimeisinValinnanvaihe(
      String hakuOid, String hakukohdeOid, int jarjestysnumero) {
    if (jarjestysnumero > 0) {
      return Optional.ofNullable(
              datastore
                  .find(ValinnanvaiheMigrationDTO.class)
                  .filter(
                      and(
                          eq("hakuOid", hakuOid),
                          eq("hakukohdeOid", hakukohdeOid),
                          lt("jarjestysnumero", jarjestysnumero)))
                  .iterator(new FindOptions().sort(descending("jarjestysnumero")).limit(1))
                  .tryNext())
          .map(this::migrate)
          .orElse(null);
    } else {
      return null;
    }
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
  public List<Valinnanvaihe> haeValinnanvaiheetJarjestysnumerolla(
      String hakuOid, String hakukohdeOid, int jarjestysnumero) {
    return migrate(
        datastore
            .find(ValinnanvaiheMigrationDTO.class)
            .filter(
                and(
                    eq("hakuOid", hakuOid),
                    eq("hakukohdeOid", hakukohdeOid),
                    eq("jarjestysnumero", jarjestysnumero)))
            .stream()
            .collect(Collectors.toList()));
  }

  @Override
  public void saveOrUpdate(Valinnanvaihe valinnanvaihe) {
    valinnanvaihe.reportDuplicateValintatapajonoOids();
    valinnanvaihe
        .getValintatapajonot()
        .forEach(
            valintatapajono -> {
              saveJonosijat(valintatapajono);
              datastore.save(valintatapajono);
            });
    datastore.save(valinnanvaihe);
  }

  @Override
  public void poistaValinnanvaihe(Valinnanvaihe valinnanvaihe) {
    valinnanvaihe.getValintatapajonot().forEach(this::poistaJono);
    datastore.delete(valinnanvaihe);
  }

  @Override
  public void poistaJono(Valintatapajono jono) {
    List<ObjectId> jonosijaIdt = jono.getJonosijaIdt();
    if (!jonosijaIdt.isEmpty()) {
      datastore.find(Jonosija.class).filter(in("_id", jonosijaIdt)).delete();
    }
    datastore.delete(jono);
  }

  // TODO tämä ei vielä oikeasti poista mitään, logittaa vain poistettavien jonosijojen id:t.
  // (BUG1899)
  @Override
  public void poistaJononJonosijatHakemusOideilla(
      Valintatapajono jono, List<String> hakemusOidsToRemove) {
    if (!hakemusOidsToRemove.isEmpty()) {
      LOGGER.warn("Poistetaan jonosijoja hakemuksille {}", hakemusOidsToRemove);
      List<ObjectId> tamanJononJonosijaIdt = jono.getJonosijaIdt();
      List<Jonosija> poistettavatJonosijat =
          datastore
              .find(Jonosija.class)
              .filter(and(in("_id", tamanJononJonosijaIdt), in("hakemusOid", hakemusOidsToRemove)))
              .stream()
              .collect(Collectors.toList());

      for (Jonosija j : poistettavatJonosijat) {
        LOGGER.warn(
            "Ollaan valmiita poistamaan hakemuksen {} jonosija ObjectId:llä {} ",
            j.getHakemusOid(),
            j.getId());
      }
    }
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
      datastore.save(migratedJono);
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
            .map(jono -> migrate(jono))
            .collect(Collectors.toList()));
    return migrated;
  }

  private List<Valinnanvaihe> migrate(List<ValinnanvaiheMigrationDTO> vaiheet) {
    return vaiheet.stream().map(this::migrate).collect(Collectors.toList());
  }
}
