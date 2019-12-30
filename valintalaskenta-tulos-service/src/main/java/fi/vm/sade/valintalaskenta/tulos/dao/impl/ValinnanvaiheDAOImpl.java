package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import fi.vm.sade.auditlog.Changes;
import fi.vm.sade.auditlog.User;
import fi.vm.sade.valinta.sharedutils.ValintaResource;
import fi.vm.sade.valinta.sharedutils.ValintaperusteetOperation;
import fi.vm.sade.valintalaskenta.domain.valinta.*;
import fi.vm.sade.valintalaskenta.tulos.LaskentaAudit;
import fi.vm.sade.valintalaskenta.tulos.dao.ValinnanvaiheDAO;
import fi.vm.sade.valintalaskenta.tulos.logging.LaskentaAuditLog;
import org.apache.commons.lang3.tuple.Pair;
import org.bson.types.ObjectId;
import org.mongodb.morphia.Datastore;
import org.mongodb.morphia.Key;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Repository;

import java.util.*;
import java.util.stream.Collectors;

@Repository
public class ValinnanvaiheDAOImpl implements ValinnanvaiheDAO {
    private static final Logger LOGGER = LoggerFactory.getLogger(ValinnanvaiheDAOImpl.class);

    @Qualifier("datastore2")
    @Autowired
    private Datastore datastore;

    @Autowired
    private LaskentaAuditLog auditLog;


    @Override
    public List<Valinnanvaihe> readByHakuOidAndHakemusOid(String hakuOid, String hakemusOid, User auditUser) {
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
    public Valinnanvaihe findByValintatapajonoOid(String valintatapajonoOid, User auditUser) {
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
    public List<Valinnanvaihe> readByHakukohdeOid(String hakukohdeoid, User auditUser) {
        return migrate(datastore.createQuery(ValinnanvaiheMigrationDTO.class)
                .field("hakukohdeOid").equal(hakukohdeoid)
                .asList());
    }

    @Override
    public List<Valinnanvaihe> readByHakuOid(String hakuoid, User auditUser) {
        return migrate(datastore.createQuery(ValinnanvaiheMigrationDTO.class)
                .field("hakuOid").equal(hakuoid)
                .asList());
    }

    @Override
    public List<ValintatapajonoMigrationDTO> valintatapajonotJotkaEivatKaytaLaskentaa() {
        return datastore.createQuery(ValintatapajonoMigrationDTO.class)
                .field("kaytetaanValintalaskentaa").notEqual(true)
                .retrievedFields(true, "_id")
                .asList();
    }

    @Override
    public List<Pair<String, String>> hakuOidHakukohdeOidPairsForJonos(List<ValintatapajonoMigrationDTO> validValintatapajonos) {
        return datastore.createQuery(ValinnanvaiheMigrationDTO.class)
                .field("valintatapajonot").in(validValintatapajonos)
                .retrievedFields(true, "hakukohdeOid", "hakuOid")
                .asList().stream()
                .map(vv -> Pair.of(vv.getHakuOid(), vv.getHakukohdeOid()))
                .distinct()
                .collect(Collectors.toList());
    }

    @Override
    public Valinnanvaihe haeValinnanvaihe(String valinnanvaiheOid, User auditUser) {
        return Optional.ofNullable(datastore.find(ValinnanvaiheMigrationDTO.class)
                .field("valinnanvaiheOid").equal(valinnanvaiheOid)
                .get())
                .map(vaihe -> migrate(vaihe))
                .orElse(null);
    }

    @Override
    public void saveOrUpdate(Valinnanvaihe vaihe, User auditUser) {
        vaihe.getValintatapajonot().forEach(valintatapajono -> {
            saveJonosijat(valintatapajono, auditUser);
            saveJono(valintatapajono, auditUser);
        });
        saveVaihe(vaihe, auditUser);
    }

    private void saveJonosijat(Valintatapajono valintatapajono, User auditUser) {
        valintatapajono.setJonosijaIdt(valintatapajono.getJonosijat().stream()
                .map(jonosija -> (ObjectId) saveJonosija(jonosija, valintatapajono, auditUser).getId())
                .collect(Collectors.toList()));
    }

    private void saveJonosijatWithoutAuditLogging(Valintatapajono valintatapajono) {
        valintatapajono.setJonosijaIdt(valintatapajono.getJonosijat().stream()
                .map(jonosija -> (ObjectId) saveJonosijaWithoutAuditLogging(jonosija).getId())
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
            saveJonosijatWithoutAuditLogging(migratedJono);
            saveJonoWithoutAuditLogging(migratedJono);
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
        return vaiheet.stream().map(vaihe -> migrate(vaihe)).collect(Collectors.toList());
    }

    private Key<Valintatapajono> saveJono(Valintatapajono valintatapajono, User auditUser) {
        auditLog.log(LaskentaAudit.AUDIT,
                auditUser,
                ValintaperusteetOperation.VALINTATAPAJONO_PAIVITYS,
                ValintaResource.VALINTATAPAJONO,
                valintatapajono.getValintatapajonoOid(),
                Changes.addedDto(valintatapajono));
        return datastore.save(valintatapajono);
    }

    private Key<Valintatapajono> saveJonoWithoutAuditLogging(Valintatapajono valintatapajono) {
        return datastore.save(valintatapajono);
    }

    private Key<Valinnanvaihe> saveVaihe(Valinnanvaihe vaihe, User auditUser) {
        auditLog.log(LaskentaAudit.AUDIT,
                auditUser,
                ValintaperusteetOperation.VALINNANVAIHE_PAIVITYS,
                ValintaResource.VALINNANVAIHE,
                vaihe.getValinnanvaiheOid(),
                Changes.addedDto(vaihe));
        return datastore.save(vaihe);
    }

    private Key<Valinnanvaihe> saveVaiheWithoutAuditLogging(Valinnanvaihe vaihe) {
        return datastore.save(vaihe);
    }

    private Key<Jonosija> saveJonosija(Jonosija jonosija, Valintatapajono valintatapajono, User auditUser) {
        auditLog.log(LaskentaAudit.AUDIT,
                auditUser,
                ValintaperusteetOperation.JONOSIJA_PAIVITYS,
                ValintaResource.JONOSIJA,
                jonosija.getId().toString(),
                Changes.addedDto(jonosija));
        return datastore.save(jonosija);
    }

    private Key<Jonosija> saveJonosijaWithoutAuditLogging(Jonosija jonosija) {
        return datastore.save(jonosija);
    }

}
