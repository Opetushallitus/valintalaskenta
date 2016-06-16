package fi.vm.sade.valintalaskenta.laskenta.dao.impl;

import fi.vm.sade.valintalaskenta.domain.valinta.*;
import org.bson.types.ObjectId;
import org.mongodb.morphia.Datastore;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValinnanvaiheDAO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import javax.annotation.PostConstruct;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Repository("valinnanvaiheDAO")
public class ValinnanvaiheDAOImpl implements ValinnanvaiheDAO {
    private static final Logger LOGGER = LoggerFactory.getLogger(ValinnanvaiheDAOImpl.class);

    @Autowired
    private Datastore datastore;

    @PostConstruct
    public void ensureIndexes() {
        datastore.ensureIndexes(Valinnanvaihe.class);
        datastore.ensureIndexes(Valintatapajono.class);
    }

    @Override
    public Valinnanvaihe haeEdeltavaValinnanvaihe(String hakuOid, String hakukohdeOid, int jarjestysnumero) {
        if (jarjestysnumero > 0) {
            return Optional.ofNullable(datastore.find(ValinnanvaiheMigrationDTO.class)
                    .field("hakuOid").equal(hakuOid)
                    .field("hakukohdeOid").equal(hakukohdeOid)
                    .field("jarjestysnumero").equal(jarjestysnumero - 1)
                    .limit(1)
                    .get())
                    .map(this::migrate)
                    .orElse(null);
        } else {
            return null;
        }
    }

    @Override
    public Valinnanvaihe haeViimeisinValinnanvaihe(String hakuOid, String hakukohdeOid, int jarjestysnumero) {
        if (jarjestysnumero > 0) {
            return Optional.ofNullable(datastore.find(ValinnanvaiheMigrationDTO.class)
                    .field("hakuOid").equal(hakuOid)
                    .field("hakukohdeOid").equal(hakukohdeOid)
                    .field("jarjestysnumero").lessThan(jarjestysnumero)
                    .order("-jarjestysnumero")
                    .limit(1)
                    .get())
                    .map(this::migrate)
                    .orElse(null);
        } else {
            return null;
        }
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
    public List<Valinnanvaihe> haeValinnanvaiheetJarjestysnumerolla(String hakuOid, String hakukohdeOid, int jarjestysnumero) {
        return migrate(datastore.find(ValinnanvaiheMigrationDTO.class)
                .field("hakuOid").equal(hakuOid)
                .field("hakukohdeOid").equal(hakukohdeOid)
                .field("jarjestysnumero").equal(jarjestysnumero)
                .asList());
    }

    @Override
    public void saveOrUpdate(Valinnanvaihe valinnanvaihe) {
        valinnanvaihe.getValintatapajonot().forEach(valintatapajono -> {
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
        datastore.delete(datastore.createQuery(Jonosija.class).field("_id").in(jono.getJonosijaIdt()));
        datastore.delete(jono);
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
                .map(jono -> migrate(jono))
                .collect(Collectors.toList()));
        return migrated;
    }

    private List<Valinnanvaihe> migrate(List<ValinnanvaiheMigrationDTO> vaiheet) {
        return vaiheet.stream().map(this::migrate).collect(Collectors.toList());
    }
}
