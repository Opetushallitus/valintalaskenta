package fi.vm.sade.valintalaskenta.laskenta.dao.impl;

import static dev.morphia.query.filters.Filters.*;
import static java.util.Comparator.comparing;

import dev.morphia.Datastore;
import fi.vm.sade.auditlog.Changes;
import fi.vm.sade.auditlog.User;
import fi.vm.sade.valinta.sharedutils.ValintaResource;
import fi.vm.sade.valinta.sharedutils.ValintaperusteetOperation;
import fi.vm.sade.valintalaskenta.domain.valinta.Hakijaryhma;
import fi.vm.sade.valintalaskenta.domain.valinta.HakijaryhmaMigrationDTO;
import fi.vm.sade.valintalaskenta.domain.valinta.Jonosija;
import fi.vm.sade.valintalaskenta.laskenta.dao.HakijaryhmaDAO;
import fi.vm.sade.valintalaskenta.tulos.LaskentaAudit;
import fi.vm.sade.valintalaskenta.tulos.logging.LaskentaAuditLog;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import org.bson.types.ObjectId;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

@Repository("hakijaryhmaDAO")
public class HakijaryhmaDAOImpl implements HakijaryhmaDAO {
  private static final Logger LOGGER = LoggerFactory.getLogger(HakijaryhmaDAOImpl.class);

  @Autowired private Datastore datastore;

  @Autowired private LaskentaAuditLog auditLog;

  @Override
  public Optional<Hakijaryhma> haeHakijaryhma(String hakijaryhmaOid) {
    return Optional.ofNullable(
            datastore
                .find(HakijaryhmaMigrationDTO.class)
                .filter(eq("hakijaryhmaOid", hakijaryhmaOid))
                .first())
        .map(this::migrateOne);
  }

  @Override
  public List<Hakijaryhma> haeHakijaryhmatPrioriteetilla(String hakukohdeOid, int prioriteetti) {
    return migrateMany(
        datastore
            .find(HakijaryhmaMigrationDTO.class)
            .filter(and(eq("hakukohdeOid", hakukohdeOid)), eq("prioriteetti", prioriteetti))
            .stream()
            .collect(Collectors.toList()));
  }

  @Override
  public List<Hakijaryhma> haeHakijaryhmat(String hakukohdeOid) {
    return migrateMany(
        datastore
            .find(HakijaryhmaMigrationDTO.class)
            .filter(eq("hakukohdeOid", hakukohdeOid))
            .stream()
            .collect(Collectors.toList()));
  }

  @Override
  public void create(Hakijaryhma hakijaryhma, User auditUser) {
    saveJonosijat(hakijaryhma, auditUser);
    auditLog.log(
        LaskentaAudit.AUDIT,
        auditUser,
        ValintaperusteetOperation.HAKIJARYHMA_PAIVITYS,
        ValintaResource.HAKIJARYHMA,
        hakijaryhma.getHakijaryhmatyyppikoodiUri(),
        Changes.addedDto(hakijaryhma));
    datastore.save(hakijaryhma);
  }

  @Override
  public void createWithoutAuditLogging(Hakijaryhma hakijaryhma) {
    saveJonosijatWithoutAuditLogging(hakijaryhma);
    datastore.save(hakijaryhma);
  }

  @Override
  public void poistaHakijaryhma(Hakijaryhma hakijaryhma) {
    List<ObjectId> jonosijaIdt = hakijaryhma.getJonosijaIdt();
    if (!jonosijaIdt.isEmpty()) {
      datastore.find(Jonosija.class).filter(in("_id", jonosijaIdt)).delete();
    }
    datastore.delete(hakijaryhma);
  }

  private void populateJonosijat(Hakijaryhma ryhma) {
    List<ObjectId> jonosijaIdt = ryhma.getJonosijaIdt();
    if (jonosijaIdt.isEmpty()) {
      ryhma.setJonosijat(new ArrayList<>());
    } else {
      ryhma.setJonosijat(
          datastore.find(Jonosija.class).filter(in("_id", jonosijaIdt)).stream()
              .collect(Collectors.toList()));
    }
  }

  private void saveJonosijat(Hakijaryhma ryhma, User auditUser) {
    ryhma.setJonosijaIdt(
        ryhma.getJonosijat().stream()
            .map(jonosija -> saveJonosija(jonosija, auditUser).getId())
            .collect(Collectors.toList()));
  }

  private void saveJonosijatWithoutAuditLogging(Hakijaryhma ryhma) {
    ryhma.setJonosijaIdt(
        ryhma.getJonosijat().stream()
            .map(jonosija -> saveJonosijaWithoutAuditLogging(jonosija).getId())
            .collect(Collectors.toList()));
  }

  private Jonosija saveJonosija(Jonosija jonosija, User auditUser) {
    auditLog.log(
        LaskentaAudit.AUDIT,
        auditUser,
        ValintaperusteetOperation.JONOSIJA_PAIVITYS,
        ValintaResource.JONOSIJA,
        jonosija.getHakemusOid(),
        Changes.addedDto(jonosija));
    return datastore.save(jonosija);
  }

  private Jonosija saveJonosijaWithoutAuditLogging(Jonosija jonosija) {
    return datastore.save(jonosija);
  }

  private Hakijaryhma migrateOne(HakijaryhmaMigrationDTO ryhma) {
    if (ryhma.getSchemaVersion() == Hakijaryhma.CURRENT_SCHEMA_VERSION) {
      Hakijaryhma alreadyMigratedRyhma =
          datastore.find(Hakijaryhma.class).filter(eq("_id", ryhma.getId())).first();
      populateJonosijat(alreadyMigratedRyhma);
      return alreadyMigratedRyhma;
    } else {
      LOGGER.info("Migrating hakijaryhma {}", ryhma.getHakijaryhmaOid());
      Hakijaryhma migratedRyhma = ryhma.migrate();
      createWithoutAuditLogging(migratedRyhma);
      return migratedRyhma;
    }
  }

  private List<Hakijaryhma> migrateMany(List<HakijaryhmaMigrationDTO> ryhmat) {
    return ryhmat.stream()
        .map(this::migrateOne)
        .sorted(comparing(Hakijaryhma::getPrioriteetti))
        .collect(Collectors.toList());
  }
}
