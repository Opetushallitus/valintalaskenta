package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import com.google.code.morphia.Datastore;
import fi.vm.sade.valintalaskenta.domain.valinta.MuokattuJonosija;
import fi.vm.sade.valintalaskenta.tulos.dao.MuokattuJonosijaDAO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: kkammone
 * Date: 5.8.2013
 * Time: 15:04
 * To change this template use File | Settings | File Templates.
 */
@Component
public class MuokattuJonosijaDAOImpl implements MuokattuJonosijaDAO {

    private static final Logger LOGGER = LoggerFactory.getLogger(MuokattuJonosijaDAOImpl.class);

    @Autowired
    private Datastore datastore;

  /*  @Override
    public List<MuokattuJonosija> readByValintatapajonoOid(String valintatapajonoOid) {
        return datastore.find(MuokattuJonosija.class)
                .filter("valintatapajonoOid", valintatapajonoOid)
                .asList();
    }  */

    @Override
    public List<MuokattuJonosija> readByHakuOid(String hakuOid) {
        return datastore.find(MuokattuJonosija.class)
                .filter("hakuOid", hakuOid)
                .asList();
    }

    @Override
    public List<MuokattuJonosija> readByhakukohdeOid(String hakukohdeOid) {
        return datastore.find(MuokattuJonosija.class)
                .filter("hakukohdeOid", hakukohdeOid)
                .asList();
    }


    @Override
    public MuokattuJonosija readByValintatapajonoOid(String valintatapajonoOid, String hakemusOid) {
        return datastore.find(MuokattuJonosija.class)
                .filter("valintatapajonoOid", valintatapajonoOid)
                .filter("hakemusOid", hakemusOid)
                .get();
    }

    @Override
    public void saveOrUpdate(MuokattuJonosija muokattuJonosija) {
        datastore.save(muokattuJonosija);
    }
}
