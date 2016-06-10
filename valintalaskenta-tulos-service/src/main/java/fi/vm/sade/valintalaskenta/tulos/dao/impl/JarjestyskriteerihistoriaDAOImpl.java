package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

import fi.vm.sade.valintalaskenta.domain.valinta.*;
import fi.vm.sade.valintalaskenta.tulos.dao.util.JarjestyskriteeriKooderi;
import org.apache.commons.io.IOUtils;
import org.bson.types.BasicBSONList;
import org.bson.types.ObjectId;
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
    public List<Jarjestyskriteerihistoria> findByValintatapajonoAndHakemusOid(String valintatapajonoOid, String hakemusOid) {
        List<ObjectId> jononJonosijaIdt = new LinkedList<>();
        datastore.find(ValintatapajonoMigrationDTO.class)
                .field("valintatapajonoOid").equal(valintatapajonoOid)
                .forEach(valintatapajono -> jononJonosijaIdt.addAll(migrate(valintatapajono).getJonosijaIdt()));
        List<ObjectId> historiaIdt = new LinkedList<>();
        datastore.find(Jonosija.class)
                .field("hakemusOid").equal(hakemusOid)
                .field("_id").in(jononJonosijaIdt)
                .forEach(jonosija -> {
                    jonosija.getJarjestyskriteeritulokset().forEach(jarjestyskriteeritulos -> {
                        historiaIdt.add(jarjestyskriteeritulos.getHistoria());
                    });
                });
        return hae(historiaIdt);
    }

    private List<Jarjestyskriteerihistoria> hae(List<ObjectId> historiaIds) {
        List<Jarjestyskriteerihistoria> historiat = datastore.createQuery(Jarjestyskriteerihistoria.class).field("_id").hasAnyOf(historiaIds).asList();
        historiat.stream().filter(JarjestyskriteeriKooderi::tarvitseekoEnkoodata).map(JarjestyskriteeriKooderi::enkoodaa).forEach(datastore::save);
        return historiat.stream().map(JarjestyskriteeriKooderi::dekoodaa).collect(Collectors.toList());
    }

    private void saveJonosijat(Valintatapajono valintatapajono) {
        valintatapajono.setJonosijaIdt(valintatapajono.getJonosijat().stream()
                .map(jonosija -> (ObjectId) datastore.save(jonosija).getId())
                .collect(Collectors.toList()));
    }

    private void populateJonosijat(Valintatapajono valintatapajono) {
        valintatapajono.setJonosijat(datastore.createQuery(Jonosija.class)
                .field("_id").in(valintatapajono.getJonosijaIdt())
                .asList());
    }

    private Valintatapajono migrate(ValintatapajonoMigrationDTO jono) {
        if (null == jono.getJonosijat()) {
            Valintatapajono alreadyMigrated = datastore.find(Valintatapajono.class)
                    .field("_id").equal(jono.getId())
                    .get();
            populateJonosijat(alreadyMigrated);
            return alreadyMigrated;
        } else {
            LOG.info("Migrating valintatapajono {}", jono.getValintatapajonoOid());
            Valintatapajono migratedJono = jono.migrate();
            saveJonosijat(migratedJono);
            datastore.save(migratedJono);
            return migratedJono;
        }
    }
}
