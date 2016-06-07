package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

import fi.vm.sade.valintalaskenta.domain.valinta.Jonosija;
import fi.vm.sade.valintalaskenta.domain.valinta.Valintatapajono;
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

import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteerihistoria;
import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
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
        datastore.find(Valintatapajono.class)
                .field("valintatapajonoOid").equal(valintatapajonoOid)
                .fetch().forEach(valintatapajono -> jononJonosijaIdt.addAll(valintatapajono.getJonosijaIdt()));
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

}
