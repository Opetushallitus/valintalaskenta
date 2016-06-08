package fi.vm.sade.valintalaskenta.laskenta.dao.impl;

import fi.vm.sade.service.valintaperusteet.resource.HakijaryhmaResource;
import fi.vm.sade.valintalaskenta.domain.valinta.Hakijaryhma;
import fi.vm.sade.valintalaskenta.domain.valinta.HakijaryhmaMigrationDTO;
import fi.vm.sade.valintalaskenta.domain.valinta.Jonosija;
import fi.vm.sade.valintalaskenta.laskenta.dao.HakijaryhmaDAO;
import org.bson.types.ObjectId;
import org.mongodb.morphia.Datastore;
import org.mongodb.morphia.Key;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import javax.annotation.PostConstruct;
import java.util.LinkedList;
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
        hakijaryhma.getJonosijat().forEach(datastore::delete);
        datastore.delete(hakijaryhma);
    }

    private void populateJonosijat(Hakijaryhma ryhma) {
        ryhma.setJonosijat(datastore.createQuery(Jonosija.class)
                .field("_id").in(ryhma.getJonosijaIdt())
                .asList());
    }

    private void saveJonosijat(Hakijaryhma ryhma) {
        ryhma.setJonosijaIdt(ryhma.getJonosijat().stream()
                .map(jonosija -> (ObjectId) datastore.save(jonosija).getId())
                .collect(Collectors.toList()));
    }

    private Hakijaryhma migrateOne(HakijaryhmaMigrationDTO ryhma) {
        if (null == ryhma.getJonosijat()) {
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
