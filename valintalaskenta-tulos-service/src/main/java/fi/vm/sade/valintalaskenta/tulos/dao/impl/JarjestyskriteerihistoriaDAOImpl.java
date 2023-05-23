package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import static dev.morphia.query.filters.Filters.*;

import dev.morphia.Datastore;
import fi.vm.sade.valintalaskenta.domain.valinta.*;
import fi.vm.sade.valintalaskenta.tulos.dao.JarjestyskriteerihistoriaDAO;
import fi.vm.sade.valintalaskenta.tulos.dao.util.JarjestyskriteeriKooderi;
import java.util.*;
import java.util.stream.Collectors;
import org.bson.types.ObjectId;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Repository;

@Repository("jonosijaHistoriaTulosDAO")
public class JarjestyskriteerihistoriaDAOImpl implements JarjestyskriteerihistoriaDAO {
  private static final Logger LOG = LoggerFactory.getLogger(JarjestyskriteerihistoriaDAOImpl.class);

  @Qualifier("datastore2")
  @Autowired
  private Datastore datastore;

  @Override
  public List<Jarjestyskriteerihistoria> findByValintatapajonoAndHakemusOid(
      String valintatapajonoOid, String hakemusOid) {
    List<ObjectId> jononJonosijaIdt = new LinkedList<>();
    datastore
        .find(ValintatapajonoMigrationDTO.class)
        .filter(eq("valintatapajonoOid", valintatapajonoOid))
        .forEach(
            valintatapajono -> jononJonosijaIdt.addAll(migrate(valintatapajono).getJonosijaIdt()));
    List<ObjectId> historiaIdt = new LinkedList<>();
    if (!jononJonosijaIdt.isEmpty()) {
      datastore
          .find(Jonosija.class)
          .filter(and(eq("hakemusOid", hakemusOid), in("_id", jononJonosijaIdt)))
          .forEach(
              jonosija -> {
                jonosija
                    .getJarjestyskriteeritulokset()
                    .forEach(
                        jarjestyskriteeritulos -> {
                          historiaIdt.add(jarjestyskriteeritulos.getHistoria());
                        });
              });
    }
    return hae(historiaIdt);
  }

  private List<Jarjestyskriteerihistoria> hae(List<ObjectId> historiaIds) {
    if (historiaIds.isEmpty()) {
      return new ArrayList<>();
    }
    List<Jarjestyskriteerihistoria> historiat =
        datastore.find(Jarjestyskriteerihistoria.class).filter(in("_id", historiaIds)).stream()
            .collect(Collectors.toList());
    historiat.stream()
        .filter(JarjestyskriteeriKooderi::tarvitseekoEnkoodata)
        .map(JarjestyskriteeriKooderi::enkoodaa)
        .forEach(datastore::save);
    return historiat.stream().map(JarjestyskriteeriKooderi::dekoodaa).collect(Collectors.toList());
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
      LOG.info("Migrating valintatapajono {}", jono.getValintatapajonoOid());
      Valintatapajono migratedJono = jono.migrate();
      saveJonosijat(migratedJono);
      datastore.save(migratedJono);
      return migratedJono;
    }
  }
}
