package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import fi.vm.sade.valintalaskenta.domain.valinta.Jonosija;
import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.valinta.ValinnanvaiheMigrationDTO;
import fi.vm.sade.valintalaskenta.domain.valinta.Valintatapajono;
import fi.vm.sade.valintalaskenta.domain.valinta.ValintatapajonoMigrationDTO;
import fi.vm.sade.valintalaskenta.tulos.dao.ValinnanvaiheDAO;
import org.bson.types.ObjectId;
import org.mongodb.morphia.Datastore;
import org.mongodb.morphia.Key;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Repository
public class ValinnanvaiheDAOImpl implements ValinnanvaiheDAO {
    private static final Logger LOGGER = LoggerFactory.getLogger(ValinnanvaiheDAOImpl.class);

    @Qualifier("datastore2")
    @Autowired
    private Datastore datastore;

    @Override
    public List<Valinnanvaihe> readByHakuOidAndHakemusOid(String hakuOid,
                                                          String hakemusOid) {
        List<ObjectId> hakemuksenJonosijaIdt = new LinkedList<>();
        datastore.find(Jonosija.class)
                .field("hakemusOid").equal(hakemusOid)
                .fetchKeys().forEach(key -> hakemuksenJonosijaIdt.add((ObjectId) key.getId()));
        if (hakemuksenJonosijaIdt.isEmpty()) {
            return new ArrayList<>();
        }
        List<Key<Valintatapajono>> hakemuksenJonot = datastore.find(Valintatapajono.class)
                .field("jonosijaIdt").in(hakemuksenJonosijaIdt)
                .asKeyList();
        if (hakemuksenJonot.isEmpty()) {
            return new ArrayList<>();
        }
        return migrate(datastore.createQuery(ValinnanvaiheMigrationDTO.class)
                .field("hakuOid").equal(hakuOid)
                .field("valintatapajonot").in(hakemuksenJonot)
                .asList());
    }

    @Override
    public Valinnanvaihe findByValintatapajonoOid(String valintatapajonoOid) {
        List<Key<Valintatapajono>> keys = datastore.find(Valintatapajono.class)
                .field("valintatapajonoOid").equal(valintatapajonoOid)
                .asKeyList();
        if (keys.isEmpty()) {
            return null;
        }
        return migrate(datastore.createQuery(ValinnanvaiheMigrationDTO.class)
                .field("valintatapajonot").in(keys)
                .get());
    }

    @Override
    public List<Valinnanvaihe> readByHakukohdeOid(String hakukohdeoid) {
        return migrate(datastore.createQuery(ValinnanvaiheMigrationDTO.class)
                .field("hakukohdeOid").equal(hakukohdeoid)
                .asList());
    }

    @Override
    public List<Valinnanvaihe> readByHakuOid(String hakuoid) {
        return migrate(datastore.createQuery(ValinnanvaiheMigrationDTO.class)
                .field("hakuOid").equal(hakuoid)
                .asList());
    }

    @Override
    public Valinnanvaihe haeValinnanvaihe(String valinnanvaiheOid) {
        return Optional.ofNullable(datastore.find(ValinnanvaiheMigrationDTO.class)
                .field("valinnanvaiheOid").equal(valinnanvaiheOid)
                .get())
                .map(this::migrate)
                .orElse(null);
    }

    @Override
    public void saveOrUpdate(Valinnanvaihe vaihe) {
        vaihe.getValintatapajonot().forEach(valintatapajono -> {
            saveJonosijat(valintatapajono);
            datastore.save(valintatapajono);
        });
        datastore.save(vaihe);
    }

    private void saveJonosijat(Valintatapajono valintatapajono) {
        valintatapajono.setJonosijaIdt(valintatapajono.getJonosijat().stream()
                .map(jonosija -> (ObjectId) datastore.save(jonosija).getId())
                .collect(Collectors.toList()));
    }

    private void populateJonosijat(Valintatapajono valintatapajono) {
        List<ObjectId> jonosijaIdt = valintatapajono.getJonosijaIdt();
        if (jonosijaIdt.isEmpty()) {
            valintatapajono.setJonosijat(new ArrayList<>());
        } else {
            valintatapajono.setJonosijat(datastore.createQuery(Jonosija.class)
                    .field("_id").in(jonosijaIdt)
                    .asList());
        }
    }

    private Valintatapajono migrate(ValintatapajonoMigrationDTO jono) {
        if (jono.getSchemaVersion() == Valintatapajono.CURRENT_SCHEMA_VERSION) {
            Valintatapajono alreadyMigrated = datastore.find(Valintatapajono.class)
                    .field("_id").equal(jono.getId())
                    .get();
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
        migrated.setValintatapajonot(vaihe.getValintatapajonot().stream()
            .map(jono -> {
                try {
                    return migrate(jono);
                } catch (RuntimeException e) {
                    LOGGER.error(String.format("Error when calling migrate for jono %s of valinnanvaihe %s", jono.getId(), vaihe.getId()), e);
                    throw e;
                }
            })
            .collect(Collectors.toList()));
        return migrated;
    }

    private List<Valinnanvaihe> migrate(List<ValinnanvaiheMigrationDTO> vaiheet) {
        return vaiheet.stream().map(this::migrate).collect(Collectors.toList());
    }
}
