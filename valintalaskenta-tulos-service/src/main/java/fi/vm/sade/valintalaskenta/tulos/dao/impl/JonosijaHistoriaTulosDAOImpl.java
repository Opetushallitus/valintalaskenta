package fi.vm.sade.valintalaskenta.tulos.dao.impl;

import com.google.code.morphia.Datastore;
import fi.vm.sade.valintalaskenta.domain.Jonosija;
import fi.vm.sade.valintalaskenta.domain.JonosijaHistoria;
import fi.vm.sade.valintalaskenta.domain.Valintatapajono;
import fi.vm.sade.valintalaskenta.tulos.dao.JonosijaHistoriaTulosDAO;
import fi.vm.sade.valintalaskenta.tulos.dao.ValintatapajonoDAO;
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
    private ValintatapajonoDAO valintatapajonoDAO;

    @Override
    public List<JonosijaHistoria> findByValintatapajonoAndVersioAndHakemusOid(String valintatapajonoOid, String hakemusOid) {
        Valintatapajono valintatapajono = valintatapajonoDAO.findByOid(valintatapajonoOid);
        for (Jonosija jonosija : valintatapajono.getJonosijat()) {
            if(jonosija.getHakemusoid().equals(hakemusOid)) {
                return jonosija.getHistoriat();
            }
        }

        return Collections.emptyList();
    }
}
