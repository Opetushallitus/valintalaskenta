package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import com.google.code.morphia.Datastore;
import com.mongodb.*;
import fi.vm.sade.valintalaskenta.domain.JonosijaHistoria;
import fi.vm.sade.valintalaskenta.domain.Valintatapajono;
import fi.vm.sade.valintalaskenta.tulos.dao.JonosijaHistoriaTulosDAO;
import fi.vm.sade.valintalaskenta.tulos.dao.ValintatapajonoDAO;
import fi.vm.sade.valintalaskenta.tulos.dao.exception.DaoException;
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
public class JonosijaHistoriaTulosDAOImpl implements JonosijaHistoriaTulosDAO {

    @Autowired
    private ValintatapajonoDAO valintatapajonoDAO;

    @Autowired
    private Datastore datastore;

    @Override
    public List<JonosijaHistoria> findByValintatapajonoAndVersioAndHakemusOid(String valintatapajonoOid, String hakemusOid) {
        DBCollection collection = datastore.getCollection(Valintatapajono.class);
        AggregationOutput aggregation = collection.aggregate(
                // Haetaan valintatapajono oidin mukaan
                new BasicDBObject("$match", new BasicDBObject("valintatapajonooid", valintatapajonoOid)),

                // Sortataan versiot ja valitaan niistä viimeisin
                new BasicDBObject("$sort", new BasicDBObject("versio", -1)),
                new BasicDBObject("$limit", 1),

                // Puretaan jonosijat
                new BasicDBObject("$project", new BasicDBObject("jonosijat", 1)),
                new BasicDBObject("$unwind", "$jonosijat"),

                // Haetaan tietyn hakemuksen jonosija ja siihen liittyvät historiat
                new BasicDBObject("$match", new BasicDBObject("jonosijat.hakemusoid", hakemusOid)),
                new BasicDBObject("$project", new BasicDBObject("jonosijat.historiat", 1))
        );

        if (!aggregation.getCommandResult().ok()) {
            throw new DaoException(aggregation.getCommandResult().getErrorMessage());
        } else if (!aggregation.results().iterator().hasNext()) {
            return Collections.EMPTY_LIST;
        }

        // Loopataan historiaviitteet läpi ja haetaan niitä vastaavat dokumentit
        DBObject result = aggregation.results().iterator().next();
        List<DBRef> historiat = (List<DBRef>) ((DBObject) result.get("jonosijat")).get("historiat");

        List<ObjectId> historiaIds = new ArrayList<ObjectId>();
        for (DBRef ref : historiat) {
            historiaIds.add((ObjectId) ref.getId());
        }

        return datastore.createQuery(JonosijaHistoria.class)
                .field("_id").hasAnyOf(historiaIds)
                .asList();
    }
}
