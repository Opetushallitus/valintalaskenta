package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

import fi.vm.sade.auditlog.Changes;
import fi.vm.sade.auditlog.User;
import fi.vm.sade.valinta.sharedutils.ValintaResource;
import fi.vm.sade.valinta.sharedutils.ValintaperusteetOperation;
import fi.vm.sade.valintalaskenta.domain.valinta.*;
import fi.vm.sade.valintalaskenta.tulos.LaskentaAudit;
import fi.vm.sade.valintalaskenta.tulos.dao.util.JarjestyskriteeriKooderi;
import org.apache.commons.io.IOUtils;
import org.bson.types.BasicBSONList;
import org.bson.types.ObjectId;
import org.mongodb.morphia.Key;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Repository;

import org.mongodb.morphia.Datastore;
import com.mongodb.AggregationOutput;
import com.mongodb.BasicDBObject;
import com.mongodb.DBCollection;
import com.mongodb.DBObject;

import fi.vm.sade.valintalaskenta.tulos.dao.JarjestyskriteerihistoriaDAO;
import fi.vm.sade.valintalaskenta.tulos.dao.exception.DaoException;

@Repository("jonosijaHistoriaTulosDAO")
public class JarjestyskriteerihistoriaDAOImpl implements JarjestyskriteerihistoriaDAO {
    private static final Logger LOG = LoggerFactory.getLogger(JarjestyskriteerihistoriaDAOImpl.class);

    @Qualifier("datastore2")
    @Autowired
    private Datastore datastore;

    @Override
    public List<Jarjestyskriteerihistoria> findByValintatapajonoAndHakemusOid(String valintatapajonoOid, String hakemusOid, User auditUser) {
        List<ObjectId> jononJonosijaIdt = new LinkedList<>();
        datastore.find(ValintatapajonoMigrationDTO.class)
                .field("valintatapajonoOid").equal(valintatapajonoOid)
                .forEach(valintatapajono -> jononJonosijaIdt.addAll(migrate(valintatapajono, auditUser).getJonosijaIdt()));
        List<ObjectId> historiaIdt = new LinkedList<>();
        if (!jononJonosijaIdt.isEmpty()) {
            datastore.find(Jonosija.class)
                    .field("hakemusOid").equal(hakemusOid)
                    .field("_id").in(jononJonosijaIdt)
                    .forEach(jonosija -> {
                        jonosija.getJarjestyskriteeritulokset().forEach(jarjestyskriteeritulos -> {
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
        List<Jarjestyskriteerihistoria> historiat = datastore.createQuery(Jarjestyskriteerihistoria.class).field("_id").hasAnyOf(historiaIds).asList();
        historiat.stream().filter(JarjestyskriteeriKooderi::tarvitseekoEnkoodata).map(JarjestyskriteeriKooderi::enkoodaa).forEach(datastore::save);
        return historiat.stream().map(JarjestyskriteeriKooderi::dekoodaa).collect(Collectors.toList());
    }

    private void saveJonosijat(Valintatapajono valintatapajono, User auditUser) {
        valintatapajono.setJonosijaIdt(valintatapajono.getJonosijat().stream()
                .map(jonosija -> (ObjectId) saveJonosija(jonosija, auditUser).getId())
                .collect(Collectors.toList()));
    }

    private Key<Jonosija> saveJonosija(Jonosija jonosija, User auditUser) {
/*        auditLog.log(LaskentaAudit.AUDIT,
                auditUser,
                ValintaperusteetOperation.JONOSIJA_PAIVITYS,
                ValintaResource.JONOSIJA,
                jonosija.getId(),
                Changes.addedDto(jonosija));*/
        return datastore.save(jonosija);
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

    private Valintatapajono migrate(ValintatapajonoMigrationDTO jono, User auditUser) {
        if (jono.getSchemaVersion() == Valintatapajono.CURRENT_SCHEMA_VERSION) {
            Valintatapajono alreadyMigrated = datastore.find(Valintatapajono.class)
                    .field("_id").equal(jono.getId())
                    .get();
            populateJonosijat(alreadyMigrated);
            return alreadyMigrated;
        } else {
            LOG.info("Migrating valintatapajono {}", jono.getValintatapajonoOid());
            Valintatapajono migratedJono = jono.migrate();
            saveJonosijat(migratedJono, auditUser);
            datastore.save(migratedJono);
            return migratedJono;
        }
    }
}
