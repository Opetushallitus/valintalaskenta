package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import static java.util.Comparator.comparing;

import fi.vm.sade.valintalaskenta.domain.valinta.Hakijaryhma;
import fi.vm.sade.valintalaskenta.domain.valinta.HakijaryhmaMigrationDTO;
import fi.vm.sade.valintalaskenta.domain.valinta.Jonosija;
import fi.vm.sade.valintalaskenta.tulos.dao.HakijaryhmaDAO;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import org.bson.types.ObjectId;
import org.mongodb.morphia.Datastore;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Repository;

@Repository
public class HakijaryhmaDAOImpl implements HakijaryhmaDAO {
  private static final Logger LOGGER = LoggerFactory.getLogger(HakijaryhmaDAOImpl.class);

  @Qualifier("datastore2")
  @Autowired
  private Datastore datastore;

  @Override
  public List<Hakijaryhma> readByHakukohdeOid(String hakukohdeoid) {
    List<HakijaryhmaMigrationDTO> ryhmat =
        datastore
            .createQuery(HakijaryhmaMigrationDTO.class)
            .field("hakukohdeOid")
            .equal(hakukohdeoid)
            .asList();
    List<Hakijaryhma> migratedRyhmat =
        ryhmat.stream()
            .map(ryhma -> migrate(ryhma))
            .sorted(comparing(Hakijaryhma::getPrioriteetti))
            .collect(Collectors.toList());
    return migratedRyhmat;
  }

  private Hakijaryhma migrate(HakijaryhmaMigrationDTO ryhma) {
    if (ryhma.getSchemaVersion() == Hakijaryhma.CURRENT_SCHEMA_VERSION) {
      Hakijaryhma alreadyMigratedRyhma =
          datastore.createQuery(Hakijaryhma.class).field("_id").equal(ryhma.getId()).get();
      List<ObjectId> jonosijaIdt = alreadyMigratedRyhma.getJonosijaIdt();
      if (jonosijaIdt.isEmpty()) {
        alreadyMigratedRyhma.setJonosijat(new ArrayList<>());
      } else {
        alreadyMigratedRyhma.setJonosijat(
            datastore.createQuery(Jonosija.class).field("_id").in(jonosijaIdt).asList());
      }
      return alreadyMigratedRyhma;
    } else {
      LOGGER.info("Migrating hakijaryhma {}", ryhma.getHakijaryhmaOid());
      Hakijaryhma migratedRyhma = ryhma.migrate();
      migratedRyhma.setJonosijaIdt(
          migratedRyhma.getJonosijat().stream()
              .map(jonosija -> (ObjectId) datastore.save(jonosija).getId())
              .collect(Collectors.toList()));
      datastore.save(migratedRyhma);
      return migratedRyhma;
    }
  }
}
