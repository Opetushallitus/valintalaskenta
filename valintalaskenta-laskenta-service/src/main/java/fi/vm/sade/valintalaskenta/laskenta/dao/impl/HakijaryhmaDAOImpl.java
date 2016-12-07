package fi.vm.sade.valintalaskenta.laskenta.dao.impl;

import fi.vm.sade.valintalaskenta.domain.valinta.Hakijaryhma;
import fi.vm.sade.valintalaskenta.domain.valinta.HakijaryhmaMigrationDTO;
import fi.vm.sade.valintalaskenta.domain.valinta.Jonosija;
import fi.vm.sade.valintalaskenta.laskenta.dao.HakijaryhmaDAO;
import org.bson.types.ObjectId;
import org.mongodb.morphia.Datastore;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import javax.annotation.PostConstruct;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Repository("hakijaryhmaDAO")
public class HakijaryhmaDAOImpl implements HakijaryhmaDAO {
    private static final Logger LOGGER = LoggerFactory.getLogger(HakijaryhmaDAOImpl.class);

    @Autowired
    private Datastore datastore;

    @PostConstruct
    public void ensureIndexes() {
        datastore.ensureIndexes(Hakijaryhma.class);
    }

    @Override
    public Optional<Hakijaryhma> haeHakijaryhma(String hakijaryhmaOid) {
        return Optional.ofNullable(datastore.find(HakijaryhmaMigrationDTO.class)
                .field("hakijaryhmaOid").equal(hakijaryhmaOid)
                .get())
                .map(this::migrateOne);
    }

    @Override
    public List<Hakijaryhma> haeHakijaryhmatPrioriteetilla(String hakukohdeOid, int prioriteetti) {
        return migrateMany(datastore.find(HakijaryhmaMigrationDTO.class)
                .field("hakukohdeOid").equal(hakukohdeOid)
                .field("prioriteetti").equal(prioriteetti)
                .asList());
    }

    @Override
    public List<Hakijaryhma> haeHakijaryhmat(String hakukohdeOid) {
        return migrateMany(datastore.find(HakijaryhmaMigrationDTO.class)
                .field("hakukohdeOid").equal(hakukohdeOid)
                .asList());
    }

    @Override
    public void create(Hakijaryhma hakijaryhma) {
        saveJonosijat(hakijaryhma);
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

    private void saveJonosijat(Hakijaryhma ryhma) {
        ryhma.setJonosijaIdt(ryhma.getJonosijat().stream()
                .map(jonosija -> (ObjectId) datastore.save(jonosija).getId())
                .collect(Collectors.toList()));
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
            create(migratedRyhma);
            return migratedRyhma;
        }
    }

    private List<Hakijaryhma> migrateMany(List<HakijaryhmaMigrationDTO> ryhmat) {
        return ryhmat.stream().map(this::migrateOne).collect(Collectors.toList());
    }
}
