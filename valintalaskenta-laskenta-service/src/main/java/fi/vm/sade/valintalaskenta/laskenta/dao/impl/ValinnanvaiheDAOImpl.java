package fi.vm.sade.valintalaskenta.laskenta.dao.impl;

import fi.vm.sade.valintalaskenta.domain.valinta.Valintatapajono;
import org.mongodb.morphia.Datastore;
import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValinnanvaiheDAO;
import org.mongodb.morphia.query.Query;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import javax.annotation.PostConstruct;
import java.util.ArrayList;
import java.util.List;

@Repository("valinnanvaiheDAO")
public class ValinnanvaiheDAOImpl implements ValinnanvaiheDAO {
    @Autowired
    private Datastore datastore;

    @PostConstruct
    public void ensureIndexes() {
        datastore.ensureIndexes(Valinnanvaihe.class);
        datastore.ensureIndexes(Valintatapajono.class);
    }

    @Override
    public Valinnanvaihe haeEdeltavaValinnanvaihe(String hakuOid, String hakukohdeOid, int jarjestysnumero) {
        Valinnanvaihe edellinen = null;

        if (jarjestysnumero > 0) {
            edellinen = datastore.find(Valinnanvaihe.class)
                    .field("hakuOid").equal(hakuOid)
                    .field("hakukohdeOid").equal(hakukohdeOid)
                    .field("jarjestysnumero").equal(jarjestysnumero - 1)
                    .limit(1)
                    .get();
        }

        return edellinen;
    }

    @Override
    public Valinnanvaihe haeViimeisinValinnanvaihe(String hakuOid, String hakukohdeOid, int jarjestysnumero) {
        Valinnanvaihe edellinen = null;
        if (jarjestysnumero > 0) {
            edellinen = datastore.find(Valinnanvaihe.class)
                    .field("hakuOid").equal(hakuOid)
                    .field("hakukohdeOid").equal(hakukohdeOid)
                    .field("jarjestysnumero").lessThan(jarjestysnumero)
                    .order("-jarjestysnumero")
                    .limit(1)
                    .get();
        }
        return edellinen;
    }

    @Override
    public Valinnanvaihe haeValinnanvaihe(String valinnanvaiheOid) {
        return datastore.find(Valinnanvaihe.class)
                .field("valinnanvaiheOid").equal(valinnanvaiheOid)
                .get();
    }

    @Override
    public List<Valinnanvaihe> haeValinnanvaiheetJarjestysnumerolla(String hakuOid, String hakukohdeOid, int jarjestysnumero) {
        return datastore.find(Valinnanvaihe.class)
                .field("hakuOid").equal(hakuOid)
                .field("hakukohdeOid").equal(hakukohdeOid)
                .field("jarjestysnumero").equal(jarjestysnumero)
                .asList();

    }

    @Override
    public void create(Valinnanvaihe valinnanvaihe) {
        valinnanvaihe.getValintatapajonot().forEach(datastore::save);
        datastore.save(valinnanvaihe);
    }

    @Override
    public void poistaValinnanvaihe(Valinnanvaihe valinnanvaihe) {
        datastore.delete(valinnanvaihe);
    }

    @Override
    public void poistaJonot(String oid) {
        final Query<Valintatapajono> query = datastore.createQuery(Valintatapajono.class).field("valintatapajonoOid").equal(oid);
        datastore.delete(query);
    }
}
