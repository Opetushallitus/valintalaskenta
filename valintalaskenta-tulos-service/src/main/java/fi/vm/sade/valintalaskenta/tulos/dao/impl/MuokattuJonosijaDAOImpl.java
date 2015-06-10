package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;

import org.mongodb.morphia.Datastore;

import fi.vm.sade.valintalaskenta.domain.valinta.MuokattuJonosija;
import fi.vm.sade.valintalaskenta.tulos.dao.MuokattuJonosijaDAO;

import javax.annotation.PostConstruct;

@Component
public class MuokattuJonosijaDAOImpl implements MuokattuJonosijaDAO {
    private static final Logger LOGGER = LoggerFactory.getLogger(MuokattuJonosijaDAOImpl.class);

    @Qualifier("datastore2")
    @Autowired
    private Datastore datastore;

    @PostConstruct
    public void ensureIndexes() {
        datastore.ensureIndexes(MuokattuJonosija.class);
    }

    @Override
    public List<MuokattuJonosija> readByHakuOid(String hakuOid) {
        return datastore.find(MuokattuJonosija.class).filter("hakuOid", hakuOid).asList();
    }

    @Override
    public List<MuokattuJonosija> readByhakukohdeOid(String hakukohdeOid) {
        return datastore.find(MuokattuJonosija.class).filter("hakukohdeOid", hakukohdeOid).asList();
    }

    @Override
    public List<MuokattuJonosija> readByHakuOidAndHakemusOid(String hakuOid, String hakemusOid) {
        return datastore.find(MuokattuJonosija.class)
                .filter("hakuOid", hakuOid).filter("hakemusOid", hakemusOid)
                .asList();
    }

    @Override
    public MuokattuJonosija readByValintatapajonoOid(String valintatapajonoOid, String hakemusOid) {
        return datastore.find(MuokattuJonosija.class)
                .filter("valintatapajonoOid", valintatapajonoOid)
                .filter("hakemusOid", hakemusOid).get();
    }

    @Override
    public void saveOrUpdate(MuokattuJonosija muokattuJonosija) {
        datastore.save(muokattuJonosija);
    }
}
