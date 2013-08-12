package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import com.google.code.morphia.Datastore;
import fi.vm.sade.valintalaskenta.domain.Jonosija;
import fi.vm.sade.valintalaskenta.domain.JonosijaHistoria;
import fi.vm.sade.valintalaskenta.domain.Valintatapajono;
import fi.vm.sade.valintalaskenta.tulos.dao.JonosijaHistoriaTulosDAO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import java.util.Collections;
import java.util.List;

/**
 * User: tommiha
 * Date: 8/12/13
 * Time: 2:20 PM
 */
@Repository("jonosijaHistoriaTulosDAO")
public class JonosijaHistoriaTulosDAOImpl implements JonosijaHistoriaTulosDAO {

    @Autowired
    private Datastore datastore;

    @Override
    public List<JonosijaHistoria> findByValintatapajonoAndVersioAndHakemusOid(String valintatapajonoOid, Integer versio, String hakemusOid) {
        Valintatapajono valintatapajono = datastore.find(Valintatapajono.class)
                .filter("valintatapajonooid", valintatapajonoOid)
                .filter("versio", versio)
                .get();
        for (Jonosija jonosija : valintatapajono.getJonosijat()) {
            if(jonosija.getHakemusoid().equals(hakemusOid)) {
                return jonosija.getHistoriat();
            }
        }

        return Collections.emptyList();
    }
}
