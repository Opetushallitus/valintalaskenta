package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import fi.vm.sade.auditlog.Changes;
import fi.vm.sade.auditlog.User;
import fi.vm.sade.valinta.sharedutils.ValintaResource;
import fi.vm.sade.valinta.sharedutils.ValintaperusteetOperation;
import fi.vm.sade.valintalaskenta.domain.valinta.Hakijaryhma;
import fi.vm.sade.valintalaskenta.domain.valinta.HakijaryhmaMigrationDTO;
import fi.vm.sade.valintalaskenta.domain.valinta.Jonosija;
import fi.vm.sade.valintalaskenta.tulos.LaskentaAudit;
import fi.vm.sade.valintalaskenta.tulos.dao.HakijaryhmaDAO;
import fi.vm.sade.valintalaskenta.tulos.logging.LaskentaAuditLog;
import org.bson.types.ObjectId;
import org.mongodb.morphia.Datastore;
import org.mongodb.morphia.Key;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import static java.util.Comparator.comparing;

@Repository
public class HakijaryhmaDAOImpl implements HakijaryhmaDAO {
    private static final Logger LOGGER = LoggerFactory.getLogger(HakijaryhmaDAOImpl.class);

    @Qualifier("datastore2")
    @Autowired
    private Datastore datastore;

    @Autowired
    private LaskentaAuditLog auditLog;

    @Override
    public List<Hakijaryhma> readByHakukohdeOid(String hakukohdeoid, User auditUser) {
        List<HakijaryhmaMigrationDTO> ryhmat = datastore.createQuery(HakijaryhmaMigrationDTO.class)
                .field("hakukohdeOid").equal(hakukohdeoid)
                .asList();
        List<Hakijaryhma> migratedRyhmat = ryhmat.stream()
                .map(ryhma -> migrate(ryhma, auditUser))
                .sorted(comparing(Hakijaryhma::getPrioriteetti))
                .collect(Collectors.toList());
        return migratedRyhmat;
    }

    private Hakijaryhma migrate(HakijaryhmaMigrationDTO ryhma, User auditUser) {
        if (ryhma.getSchemaVersion() == Hakijaryhma.CURRENT_SCHEMA_VERSION) {
            Hakijaryhma alreadyMigratedRyhma = datastore.createQuery(Hakijaryhma.class)
                    .field("_id").equal(ryhma.getId())
                    .get();
            List<ObjectId> jonosijaIdt = alreadyMigratedRyhma.getJonosijaIdt();
            if (jonosijaIdt.isEmpty()) {
                alreadyMigratedRyhma.setJonosijat(new ArrayList<>());
            } else {
                alreadyMigratedRyhma.setJonosijat(datastore.createQuery(Jonosija.class)
                        .field("_id").in(jonosijaIdt)
                        .asList());
            }
            return alreadyMigratedRyhma;
        } else {
            LOGGER.info("Migrating hakijaryhma {}", ryhma.getHakijaryhmaOid());
            Hakijaryhma migratedRyhma = ryhma.migrate();
            migratedRyhma.setJonosijaIdt(migratedRyhma.getJonosijat().stream()
                    .map(jonosija -> (ObjectId) saveJonosija(jonosija, auditUser).getId())
                    .collect(Collectors.toList()));
            saveHakijaryhma(migratedRyhma, auditUser);
            return migratedRyhma;
        }
    }

    private Key<Jonosija> saveJonosija(Jonosija jonosija, User auditUser) {
        auditLog.log(LaskentaAudit.AUDIT,
            auditUser,
            ValintaperusteetOperation.JONOSIJA_PAIVITYS,
            ValintaResource.JONOSIJA,
            jonosija.getId().toString(),
            Changes.addedDto(jonosija));
        return datastore.save(jonosija);
    }

    private Key<Hakijaryhma> saveHakijaryhma(Hakijaryhma hakijaryhma, User auditUser) {
        auditLog.log(LaskentaAudit.AUDIT,
                auditUser,
                ValintaperusteetOperation.HAKIJARYHMA_PAIVITYS,
                ValintaResource.HAKIJARYHMA,
                hakijaryhma.getHakijaryhmatyyppikoodiUri(),
                Changes.addedDto(hakijaryhma));
        return datastore.save(hakijaryhma);
    }
}
