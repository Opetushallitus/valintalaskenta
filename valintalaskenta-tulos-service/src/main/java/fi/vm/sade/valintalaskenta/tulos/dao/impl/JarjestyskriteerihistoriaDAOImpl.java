package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import com.google.code.morphia.Datastore;
import com.mongodb.AggregationOutput;
import com.mongodb.BasicDBObject;
import com.mongodb.DBCollection;
import com.mongodb.DBObject;
import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteerihistoria;
import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.tulos.dao.JarjestyskriteerihistoriaDAO;
import fi.vm.sade.valintalaskenta.tulos.dao.exception.DaoException;
import org.bson.types.BasicBSONList;
import org.bson.types.ObjectId;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * User: tommiha
 * Date: 8/12/13
 * Time: 2:20 PM
 */
@Repository("jonosijaHistoriaTulosDAO")
public class JarjestyskriteerihistoriaDAOImpl implements JarjestyskriteerihistoriaDAO {

    @Autowired
    private Datastore datastore;

    @Override
    public List<Jarjestyskriteerihistoria> findByValintatapajonoAndVersioAndHakemusOid(String valintatapajonoOid, String hakemusOid) {
        DBCollection collection = datastore.getCollection(Valinnanvaihe.class);

        AggregationOutput aggregation = collection.aggregate(
                // Haetaan valintatapajono oidin mukaan
                new BasicDBObject("$match", new BasicDBObject("valintatapajonot.valintatapajonoOid", valintatapajonoOid)),

                // Puretaan valintatapajonot
                new BasicDBObject("$project", new BasicDBObject("valintatapajonot", 1)),
                new BasicDBObject("$unwind", "$valintatapajonot"),
                new BasicDBObject("$match", new BasicDBObject("valintatapajonot.valintatapajonoOid", valintatapajonoOid)),
                new BasicDBObject("$unwind", "$valintatapajonot.jonosijat"),

                // Haetaan tietyn hakemuksen jonosija ja siihen liittyvät historiat
                new BasicDBObject("$match", new BasicDBObject("valintatapajonot.jonosijat.hakemusOid", hakemusOid)),
                new BasicDBObject("$group", new BasicDBObject("_id", "$valintatapajonot.valintatapajonoOid")
                        .append("historiat", new BasicDBObject("$addToSet", "$valintatapajonot.jonosijat.jarjestyskriteeritulokset.historia")))
        );

        if (!aggregation.getCommandResult().ok()) {
            throw new DaoException(aggregation.getCommandResult().getErrorMessage());
        } else if (!aggregation.results().iterator().hasNext()) {
            return Collections.EMPTY_LIST;
        }

        // Loopataan historiaviitteet läpi ja haetaan niitä vastaavat dokumentit
        DBObject result = aggregation.results().iterator().next();
        BasicBSONList historiat = (BasicBSONList) ((BasicBSONList) ((DBObject) result.get("historiat"))).get(0);

        List<ObjectId> historiaIds = new ArrayList<ObjectId>();
        for (Object ref : historiat) {
            historiaIds.add((ObjectId) ref);
        }

        return datastore.createQuery(Jarjestyskriteerihistoria.class)
                .field("_id").hasAnyOf(historiaIds)
                .asList();
    }
}
