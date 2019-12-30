package fi.vm.sade.valintalaskenta.laskenta.dao.impl;

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
import org.bson.types.ObjectId;
import org.mongodb.morphia.Datastore;
import org.mongodb.morphia.Key;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import javax.annotation.PostConstruct;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import static java.util.Comparator.comparing;

@Repository("hakijaryhmaDAO")
public class HakijaryhmaDAOImpl implements HakijaryhmaDAO {
    private static final Logger LOGGER = LoggerFactory.getLogger(HakijaryhmaDAOImpl.class);

    @Autowired
    private Datastore datastore;

    @Autowired
    private LaskentaAuditLog auditLog;

    @PostConstruct
    public void ensureIndexes() {
        datastore.ensureIndexes(Hakijaryhma.class);
    }

    @Override
    public Optional<Hakijaryhma> haeHakijaryhma(String hakijaryhmaOid) {
        return Optional.ofNullable(datastore.find(HakijaryhmaMigrationDTO.class)
                .field("hakijaryhmaOid").equal(hakijaryhmaOid)
                .get())
                .map(ryhma -> migrateOne(ryhma));
    }

    @Override
    public List<Hakijaryhma> haeHakijaryhmatPrioriteetilla(String hakukohdeOid, int prioriteetti) {
        return migrateMany(datastore.find(HakijaryhmaMigrationDTO.class)
                .field("hakukohdeOid").equal(hakukohdeOid)
                .field("prioriteetti").equal(prioriteetti)
                .asList()
        );
    }

    @Override
    public List<Hakijaryhma> haeHakijaryhmat(String hakukohdeOid) {
        return migrateMany(datastore.find(HakijaryhmaMigrationDTO.class)
                .field("hakukohdeOid").equal(hakukohdeOid)
                .asList());
    }

    @Override
    public void create(Hakijaryhma hakijaryhma, User auditUser) {
        saveJonosijat(hakijaryhma, auditUser);
        auditLog.log(LaskentaAudit.AUDIT,
                auditUser,
                ValintaperusteetOperation.HAKIJARYHMA_PAIVITYS,
                ValintaResource.HAKIJARYHMA,
                hakijaryhma.getHakijaryhmatyyppikoodiUri(),
                Changes.addedDto(hakijaryhma));
        datastore.save(hakijaryhma);
    }

    private void createWithoutAuditLogging(Hakijaryhma hakijaryhma) {
        saveJonosijatWithoutAuditLogging(hakijaryhma);
        datastore.save(hakijaryhma);
    }

    @Override
    public void poistaHakijaryhma(Hakijaryhma hakijaryhma) {
        List<ObjectId> jonosijaIdt = hakijaryhma.getJonosijaIdt();
        if (!jonosijaIdt.isEmpty()) {
            datastore.delete(datastore.createQuery(Jonosija.class).field("_id").in(jonosijaIdt));
        }
        datastore.delete(hakijaryhma);
    }

    private void populateJonosijat(Hakijaryhma ryhma) {
        List<ObjectId> jonosijaIdt = ryhma.getJonosijaIdt();
        if (jonosijaIdt.isEmpty()) {
            ryhma.setJonosijat(new ArrayList<>());
        } else {
            ryhma.setJonosijat(datastore.createQuery(Jonosija.class)
                    .field("_id").in(jonosijaIdt)
                    .asList());
        }
    }

    private void saveJonosijat(Hakijaryhma ryhma, User auditUser) {
        ryhma.setJonosijaIdt(ryhma.getJonosijat().stream()
                .map(jonosija -> (ObjectId) saveJonosija(jonosija, auditUser).getId())
                .collect(Collectors.toList()));
    }

    private void saveJonosijatWithoutAuditLogging(Hakijaryhma ryhma) {
        ryhma.setJonosijaIdt(ryhma.getJonosijat().stream()
                .map(jonosija -> (ObjectId) saveJonosijaWithoutAuditLogging(jonosija).getId())
                .collect(Collectors.toList()));
    }

    private Key<Jonosija> saveJonosija(Jonosija jonosija, User auditUser) {
        auditLog.log(LaskentaAudit.AUDIT,
                auditUser,
                ValintaperusteetOperation.JONOSIJA_PAIVITYS,
                ValintaResource.JONOSIJA,
                jonosija.getHakemusOid(),
                Changes.addedDto(jonosija));
        return datastore.save(jonosija);
    }

    private Key<Jonosija> saveJonosijaWithoutAuditLogging(Jonosija jonosija) {
        return datastore.save(jonosija);
    }

    private Hakijaryhma migrateOne(HakijaryhmaMigrationDTO ryhma) {
        if (ryhma.getSchemaVersion() == Hakijaryhma.CURRENT_SCHEMA_VERSION) {
            Hakijaryhma alreadyMigratedRyhma = datastore.createQuery(Hakijaryhma.class)
                    .field("_id").equal(ryhma.getId())
                    .get();
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
                .map(ryhma -> migrateOne(ryhma))
                .sorted(comparing(Hakijaryhma::getPrioriteetti))
                .collect(Collectors.toList());
    }
}
