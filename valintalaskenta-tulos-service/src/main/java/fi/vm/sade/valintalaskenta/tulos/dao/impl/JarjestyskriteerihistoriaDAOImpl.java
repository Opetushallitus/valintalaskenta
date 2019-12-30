package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import java.util.*;
import java.util.stream.Collectors;

import fi.vm.sade.valintalaskenta.domain.valinta.*;
import fi.vm.sade.valintalaskenta.tulos.dao.util.JarjestyskriteeriKooderi;

import org.bson.types.ObjectId;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Repository;

import org.mongodb.morphia.Datastore;

import fi.vm.sade.valintalaskenta.tulos.dao.JarjestyskriteerihistoriaDAO;

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
            LOG.info("Migrating valintatapajono {}", jono.getValintatapajonoOid());
            Valintatapajono migratedJono = jono.migrate();
            saveJonosijat(migratedJono);
            datastore.save(migratedJono);
            return migratedJono;
        }
    }

}
