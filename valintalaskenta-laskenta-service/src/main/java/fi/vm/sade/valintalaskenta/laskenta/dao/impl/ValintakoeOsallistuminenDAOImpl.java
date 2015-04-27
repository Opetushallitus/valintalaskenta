package fi.vm.sade.valintalaskenta.laskenta.dao.impl;

import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import fi.vm.sade.valintalaskenta.laskenta.dao.ValintakoeOsallistuminenDAO;
import org.mongodb.morphia.Datastore;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Repository;

import javax.annotation.PostConstruct;
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

        if (jarjestysnumero > 0) {
            edellinen = morphiaDS.find(ValintakoeOsallistuminen.class)
                    .field("hakuOid").equal(hakuOid)
                    .field("hakutoiveet.hakukohdeOid").equal(hakukohdeOid)
                    .field("hakutoiveet.valinnanVaiheet.valinnanVaiheJarjestysluku").equal(jarjestysnumero - 1)
                    .limit(1)
                    .hintIndex(useIndexQueries ? ValintakoeOsallistuminen.INDEX_HAE_HAKU_KOHDE_VAIHEENJARJESTYSLUKU : null)
                    .get();
        }

        return edellinen;
    }
}
