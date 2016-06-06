package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import com.mongodb.*;
import fi.vm.sade.valintalaskenta.domain.valinta.Jonosija;
import fi.vm.sade.valintalaskenta.tulos.dao.util.MongoMapReduceUtil;
import org.bson.types.ObjectId;
import org.mongodb.morphia.Key;
import org.mongodb.morphia.mapping.Mapper;
import org.mongodb.morphia.mapping.cache.DefaultEntityCache;
import org.mongodb.morphia.query.MorphiaKeyIterator;
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

@Repository
public class ValinnanvaiheDAOImpl implements ValinnanvaiheDAO {
    private static final Logger LOGGER = LoggerFactory.getLogger(ValinnanvaiheDAOImpl.class);

    @Qualifier("datastore2")
    @Autowired
    private Datastore datastore;

    @Override
    public List<Valinnanvaihe> readByHakuOidAndHakemusOid(String hakuOid,
                                                          String hakemusOid) {
        List<ObjectId> hakemuksenJonosijaIdt = new LinkedList<>();
        datastore.find(Jonosija.class)
                .field("hakemusOid").equal(hakemusOid)
                .fetchKeys().forEach(key -> hakemuksenJonosijaIdt.add((ObjectId) key.getId()));
        List<Key<Valintatapajono>> hakemuksenJonot = datastore.find(Valintatapajono.class)
                .field("jonosijaIdt").in(hakemuksenJonosijaIdt)
                .asKeyList();
        List<Valinnanvaihe> valinnanvaiheet = datastore.createQuery(Valinnanvaihe.class)
                .field("hakuOid").equal(hakuOid)
                .field("valintatapajonot").in(hakemuksenJonot)
                .asList();
        valinnanvaiheet.forEach(this::populateJonosijat);
        return valinnanvaiheet;
    }

    @Override
    public Valinnanvaihe findByValintatapajonoOid(String valintatapajonoOid) {
        List<Key<Valintatapajono>> keys = datastore.find(Valintatapajono.class)
                .field("valintatapajonoOid").equal(valintatapajonoOid)
                .asKeyList();
        Valinnanvaihe valinnanvaihe = datastore.createQuery(Valinnanvaihe.class)
                .field("valintatapajonot").in(keys)
                .get();
        populateJonosijat(valinnanvaihe);
        return valinnanvaihe;
    }

    @Override
    public List<Valinnanvaihe> readByHakukohdeOid(String hakukohdeoid) {
        List<Valinnanvaihe> valinnanvaiheet = datastore.createQuery(Valinnanvaihe.class)
                .field("hakukohdeOid").equal(hakukohdeoid)
                .asList();
        valinnanvaiheet.forEach(this::populateJonosijat);
        return valinnanvaiheet;
    }

    @Override
    public List<Valinnanvaihe> readByHakuOid(String hakuoid) {
        List<Valinnanvaihe> valinnanvaiheet = datastore.createQuery(Valinnanvaihe.class)
                .field("hakuOid").equal(hakuoid)
                .asList();
        valinnanvaiheet.forEach(this::populateJonosijat);
        return valinnanvaiheet;
    }

    @Override
    public Valinnanvaihe haeValinnanvaihe(String valinnanvaiheOid) {
        Valinnanvaihe valinnanvaihe = datastore.find(Valinnanvaihe.class)
                .field("valinnanvaiheOid").equal(valinnanvaiheOid)
                .get();
        populateJonosijat(valinnanvaihe);
        return valinnanvaihe;
    }

    @Override
    public void saveOrUpdate(Valinnanvaihe vaihe) {
        vaihe.getValintatapajonot().forEach(valintatapajono -> {
            valintatapajono.setJonosijaIdt(valintatapajono.getJonosijat().stream()
                    .map(jonosija -> (ObjectId) datastore.save(jonosija).getId())
                    .collect(Collectors.toList()));
            datastore.save(valintatapajono);
        });
        datastore.save(vaihe);
    }

    private void populateJonosijat(Valinnanvaihe valinnanvaihe) {
        if (null != valinnanvaihe) {
            valinnanvaihe.getValintatapajonot().forEach(valintatapajono -> {
                valintatapajono.setJonosijat(datastore.createQuery(Jonosija.class)
                        .field("_id").in(valintatapajono.getJonosijaIdt())
                        .asList());
            });
        }
    }
}
