package fi.vm.sade.valintalaskenta.dao.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import com.google.code.morphia.Datastore;

import fi.vm.sade.valintalaskenta.dao.ValintatapajonoDAO;
import fi.vm.sade.valintalaskenta.domain.Valintatapajono;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
@Repository
public class ValintatapajonoDAOImpl implements ValintatapajonoDAO {

    @Autowired
    private Datastore datastore;

    public void createOrUpdate(Valintatapajono v) {
        datastore.save(v);
    }
}
