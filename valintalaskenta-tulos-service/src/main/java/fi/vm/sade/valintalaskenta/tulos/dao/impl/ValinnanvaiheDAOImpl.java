package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import com.mongodb.*;
import fi.vm.sade.valintalaskenta.tulos.dao.util.MongoMapReduceUtil;
import org.mongodb.morphia.Key;
import org.mongodb.morphia.mapping.Mapper;
import org.mongodb.morphia.mapping.cache.DefaultEntityCache;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Repository;

import org.mongodb.morphia.Datastore;
import org.mongodb.morphia.query.UpdateOperations;

import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.valinta.Valintatapajono;
import fi.vm.sade.valintalaskenta.tulos.dao.ValinnanvaiheDAO;

/**
 * @author Jussi Jartamo
 */
@Repository
public class ValinnanvaiheDAOImpl implements ValinnanvaiheDAO {

	private static final Logger LOGGER = LoggerFactory
			.getLogger(ValinnanvaiheDAOImpl.class);

	@Qualifier("datastore2")
	@Autowired
	private Datastore datastore;

	@Override
	public List<Valinnanvaihe> readByHakuOidAndHakemusOid(String hakuOid,
			String hakemusOid) {

//        final BasicDBObject query = new BasicDBObject("hakuOid", hakuOid).append("valintatapajonot.jonosijat.hakemusOid", hakemusOid);
//
//        String map = MongoMapReduceUtil.shallowCloneJs + " copy.valintatapajonot = copy.valintatapajonot.map(function(jono) { jono = shallowClone(jono); jono.jonosijat = jono.jonosijat.filter(function (hakemus) { return hakemus.hakemusOid == '"+hakemusOid+"' }); return jono }); emit(this.oid, copy) }\n";
//
//        String reduce = "function(key, values) { return values[0] }";
//
//        DBCollection collection = datastore.getCollection(Valinnanvaihe.class);
//        MapReduceCommand cmd = new MapReduceCommand(
//                collection,
//                map,
//                reduce,
//                null,
//                MapReduceCommand.OutputType.INLINE,
//                query);
//        MapReduceOutput out = collection.mapReduce(cmd);
//
//        return StreamSupport.stream(out.results().spliterator(), false)
//                .map(dbObject -> new Mapper().fromDBObject(Valinnanvaihe.class, (DBObject) dbObject.get("value"), new DefaultEntityCache()))
//                .collect(Collectors.toList());

        List<Key<Valintatapajono>> keys = datastore.find(Valintatapajono.class).field("jonosijat.hakemusOid").equal(hakemusOid).asKeyList();

        return datastore.createQuery(Valinnanvaihe.class).field("hakuOid")
                .equal(hakuOid).field("valintatapajonot").in(keys)
                .asList();

	}

	@Override
	public Valinnanvaihe findByValintatapajonoOid(String valintatapajonoOid) {
		return datastore.createQuery(Valinnanvaihe.class)
				.field("valintatapajonot.valintatapajonoOid")
				.equal(valintatapajonoOid).get();
	}

	@Override
	public List<Valinnanvaihe> readByHakukohdeOid(String hakukohdeoid) {
		return datastore.createQuery(Valinnanvaihe.class).field("hakukohdeOid")
				.equal(hakukohdeoid).asList();
	}

	@Override
	public List<Valinnanvaihe> readByHakuOid(String hakuoid) {
		return datastore.createQuery(Valinnanvaihe.class).field("hakuOid")
				.equal(hakuoid).asList();
	}

	@Override
	public void create(Valinnanvaihe valinnanvaihe) {
		datastore.save(valinnanvaihe);
	}

	@Override
	public void update(Valinnanvaihe valinnanvaihe,
			List<Valintatapajono> jonot, String hakukohdeoid, String hakuoid, String tarjoajaOid) {

        jonot.forEach(datastore::save);
        valinnanvaihe.setHakukohdeOid(hakukohdeoid);
        valinnanvaihe.setHakuOid(hakuoid);
        valinnanvaihe.setTarjoajaOid(tarjoajaOid);
        valinnanvaihe.setValintatapajonot(jonot);
        saveOrUpdate(valinnanvaihe);

//		UpdateOperations ops = datastore
//				.createUpdateOperations(Valinnanvaihe.class)
//				.set("valintatapajonot", jonot)
//				.set("hakukohdeOid", hakukohdeoid).set("hakuOid", hakuoid).set("tarjoajaOid", tarjoajaOid);
//		datastore.update(valinnanvaihe, ops);
	}

	@Override
	public Valinnanvaihe haeValinnanvaihe(String valinnanvaiheOid) {
		return datastore.find(Valinnanvaihe.class).field("valinnanvaiheOid")
				.equal(valinnanvaiheOid).get();
	}

    @Override
    public void saveOrUpdate(Valinnanvaihe vaihe) {
        vaihe.getValintatapajonot().forEach(datastore::save);
        datastore.save(vaihe);
    }
}
