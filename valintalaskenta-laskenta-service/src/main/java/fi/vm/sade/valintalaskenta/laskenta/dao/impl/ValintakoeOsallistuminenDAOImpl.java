package fi.vm.sade.valintalaskenta.laskenta.dao.impl;

import com.mongodb.AggregationOutput;
import com.mongodb.BasicDBObject;
import com.mongodb.DBCollection;
import com.mongodb.DBObject;
import fi.vm.sade.valintalaskenta.domain.valinta.Valintatapajono;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValintakoeOsallistuminenDAO;
import fi.vm.sade.valintalaskenta.tulos.dao.exception.DaoException;
import org.bson.types.BasicBSONList;
import org.bson.types.ObjectId;
import org.mongodb.morphia.Datastore;
import org.mongodb.morphia.aggregation.AggregationPipeline;
import org.mongodb.morphia.aggregation.Projection;
import org.mongodb.morphia.query.MorphiaIterator;
import org.mongodb.morphia.query.Query;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Repository;

import javax.annotation.PostConstruct;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

/**
 * User: wuoti
 * Date: 2.5.2013
 * Time: 14.05
 */
@Repository("ValintakoeOsallistuminenDAO")
public class ValintakoeOsallistuminenDAOImpl implements ValintakoeOsallistuminenDAO {

    @Value("${valintalaskenta-laskenta-service.mongodb.useIndexQueries:false}")
    private boolean useIndexQueries;

    @Autowired
    private Datastore morphiaDS;

    @PostConstruct
    public void ensureIndexes() {
        morphiaDS.ensureIndexes(ValintakoeOsallistuminen.class);
    }
    @Override
    public List<ValintakoeOsallistuminen> readAll() {
        return morphiaDS.find(ValintakoeOsallistuminen.class).asList();
    }

    @Override
    public ValintakoeOsallistuminen readByHakuOidAndHakemusOid(String hakuOid, String hakemusOid) {
        return morphiaDS.find(ValintakoeOsallistuminen.class, "hakuOid", hakuOid)
                .filter("hakemusOid", hakemusOid).get();
    }

    @Override
    public void createOrUpdate(ValintakoeOsallistuminen v) {
        morphiaDS.save(v);
    }

    @Override
    public ValintakoeOsallistuminen haeEdeltavaValinnanvaihe(String hakuOid, String hakukohdeOid, int jarjestysnumero) {
        ValintakoeOsallistuminen edellinen = null;

        final Query<ValintakoeOsallistuminen> query = morphiaDS.createQuery(ValintakoeOsallistuminen.class);
        final MorphiaIterator<ValintakoeOsallistuminen, ValintakoeOsallistuminen> edellisenVaiheenOsallistumiset = morphiaDS.<ValintakoeOsallistuminen, ValintakoeOsallistuminen>createAggregation(ValintakoeOsallistuminen.class)
                .match(query.field("hakuOid").equal(hakuOid))
                .project(Projection.projection("_id").suppress(), Projection.projection("hakutoiveet"), Projection.projection("hakuOid"))
                .unwind("hakutoiveet")
                .match(query.field("hakutoiveet.hakukohdeOid").equal(hakukohdeOid))
                .unwind("hakutoiveet.valinnanVaiheet")
                .match(query.field("hakutoiveet.valinnanVaiheet.valinnanVaiheJarjestysluku").equal(jarjestysnumero - 1))
                .limit(1)
                .aggregate(ValintakoeOsallistuminen.class);
        if (edellisenVaiheenOsallistumiset.hasNext()) {
            edellinen = edellisenVaiheenOsallistumiset.next();
        }

        return edellinen;
    }
}
