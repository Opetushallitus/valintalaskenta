package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import com.google.code.morphia.Datastore;
import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.tulos.dao.ValinnanvaiheDAO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import java.util.List;

/**
 * @author Jussi Jartamo
 */
@Repository
public class ValinnanvaiheDAOImpl implements ValinnanvaiheDAO {

    private static final Logger LOGGER = LoggerFactory.getLogger(ValinnanvaiheDAOImpl.class);

    @Autowired
    private Datastore datastore;

    @Override
    public List<Valinnanvaihe> readByHakuOidAndHakemusOid(String hakuOid, String hakemusOid) {
        // TODO: Tämän kyselyn voisi tehdä fiksummin alla olevalla aggregoinnilla. Muutetaan kun keritään.
        // DBCollection collection = datastore.getCollection(Valinnanvaihe.class);
        // collection.aggregate(
        // new BasicDBObject("$match", new BasicDBObject("hakuOid", hakuOid).append("valintatapajonot.jonosijat.hakemusOid", hakemusOid)),
        // new BasicDBObject("$unwind", "$valintatapajonot"),
        // new BasicDBObject("$unwind", "$valintatapajonot.jonosijat"),
        // new BasicDBObject("$match", new BasicDBObject("valintatapajonot.jonosijat.hakemusOid", hakemusOid)),
        // new BasicDBObject("$group", new BasicDBObject("_id",
        // new BasicDBObject("hakukohdeOid", "$hakukohdeOid").append("valinnanvaiheOid", "$valinnanvaiheOid"))
        // .append("valintatapajonot", new BasicDBObject("$addToSet", "$valintatapajonot")))
        //        );

        return datastore.createQuery(Valinnanvaihe.class)
                .field("hakuOid").equal(hakuOid)
                .field("valintatapajonot.jonosijat.hakemusOid").equal(hakemusOid)
                .asList();


    }

    @Override
    public Valinnanvaihe findByValintatapajonoOid(String valintatapajonoOid) {
        return datastore.createQuery(Valinnanvaihe.class)
                .field("valintatapajonot.valintatapajonoOid").equal(valintatapajonoOid).get();
    }

    @Override
    public List<Valinnanvaihe> readByHakukohdeOid(String hakukohdeoid) {
        return datastore.createQuery(Valinnanvaihe.class)
                .field("hakukohdeOid").equal(hakukohdeoid)
                .asList();
    }

    @Override
    public List<Valinnanvaihe> readByHakuOid(String hakuoid) {
        return datastore.createQuery(Valinnanvaihe.class)
                .field("hakuOid").equal(hakuoid)
                .asList();
    }
}
