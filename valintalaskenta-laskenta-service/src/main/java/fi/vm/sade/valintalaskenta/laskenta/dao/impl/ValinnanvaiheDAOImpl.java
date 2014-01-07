package fi.vm.sade.valintalaskenta.laskenta.dao.impl;

import com.google.code.morphia.Datastore;
import fi.vm.sade.valintalaskenta.domain.valinta.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValinnanvaiheDAO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

/**
 * User: wuoti
 * Date: 4.9.2013
 * Time: 12.02
 */
@Repository("valinnanvaiheDAO")
public class ValinnanvaiheDAOImpl implements ValinnanvaiheDAO {

    @Autowired
    private Datastore datastore;

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
    public void create(Valinnanvaihe valinnanvaihe) {
        datastore.save(valinnanvaihe);
    }


}
