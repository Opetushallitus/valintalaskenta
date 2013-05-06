package fi.vm.sade.valintalaskenta.dao.impl;

import com.google.code.morphia.Datastore;
import fi.vm.sade.valintalaskenta.dao.ValintakoeOsallistuminenDAO;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

/**
 * User: wuoti
 * Date: 2.5.2013
 * Time: 14.05
 */
@Repository("ValintakoeOsallistuminenDAO")
public class ValintakoeOsallistuminenDAOImpl implements ValintakoeOsallistuminenDAO {

    @Autowired
    private Datastore morphiaDS;

    @Override
    public ValintakoeOsallistuminen readByHakuOidAndHakemusOid(String hakuOid, String hakemusOid) {
        return morphiaDS.find(ValintakoeOsallistuminen.class, "hakuOid", hakuOid)
                .filter("hakemusOid", hakemusOid).get();
    }

    @Override
    public void createOrUpdate(ValintakoeOsallistuminen v) {
        morphiaDS.save(v);
    }
}
