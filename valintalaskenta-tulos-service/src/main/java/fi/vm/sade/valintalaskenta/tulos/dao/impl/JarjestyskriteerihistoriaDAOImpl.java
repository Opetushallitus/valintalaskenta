package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

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
    public List<Jarjestyskriteerihistoria> findByValintatapajonoAndVersioAndHakemusOid(String valintatapajonoOid, String hakemusOid) {
        DBCollection collection = datastore.getCollection(Valintatapajono.class);

        AggregationOutput aggregation = collection
                .aggregate(
                        // Haetaan valintatapajono oidin mukaan
                        new BasicDBObject("$match", new BasicDBObject("valintatapajonoOid", valintatapajonoOid)),
                        new BasicDBObject("$unwind", "$jonosijat"),

                        // Haetaan tietyn hakemuksen jonosija ja siihen
                        // liittyvät historiat
                        new BasicDBObject("$match", new BasicDBObject("jonosijat.hakemusOid", hakemusOid)),
                        new BasicDBObject("$group",
                                new BasicDBObject("_id", "$valintatapajonoOid")
                                        .append("historiat", new BasicDBObject("$addToSet", "$jonosijat.jarjestyskriteeritulokset.historia"))));

        final Iterator<DBObject> iterator = aggregation.results().iterator();
        if (!aggregation.getCommandResult().ok()) {
            throw new DaoException(aggregation.getCommandResult().getErrorMessage());
        } else if (!iterator.hasNext()) {
            return Collections.EMPTY_LIST;
        }

        // Loopataan historiaviitteet läpi ja haetaan niitä vastaavat dokumentit
        BasicBSONList historiat = new BasicBSONList();
        while (iterator.hasNext()) {
            DBObject result = iterator.next();
            final BasicBSONList current = (BasicBSONList) ((BasicBSONList) result.get("historiat")).get(0);
            if (!current.isEmpty()) {
                historiat = current;
                break;
            }
        }
        List<ObjectId> historiaIds = new ArrayList<ObjectId>();
        for (Object ref : historiat) {
            historiaIds.add((ObjectId) ref);
        }
        return hae(historiaIds);
    }

    public List<Jarjestyskriteerihistoria> hae(List<ObjectId> historiaIds) {
        List<Jarjestyskriteerihistoria> historiat = datastore.createQuery(Jarjestyskriteerihistoria.class).field("_id").hasAnyOf(historiaIds).asList();
        historiat.stream().filter(JarjestyskriteeriKooderi::tarvitseekoEnkoodata).map(JarjestyskriteeriKooderi::enkoodaa).forEach(datastore::save);
        return historiat.stream().map(JarjestyskriteeriKooderi::dekoodaa).collect(Collectors.toList());
    }

}
