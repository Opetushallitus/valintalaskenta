package fi.vm.sade.valintalaskenta.dao.impl;

import com.google.code.morphia.Datastore;
import fi.vm.sade.valintalaskenta.dao.JonosijaHistoriaDAO;
import fi.vm.sade.valintalaskenta.domain.JonosijaHistoria;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

/**
 * User: tommiha
 * Date: 8/9/13
 * Time: 10:38 AM
 */
@Repository("jonosijaHistoriaDAO")
public class JonosijaHistoriaDAOImpl implements JonosijaHistoriaDAO {

    @Autowired
    private Datastore datastore;

    @Override
    public void create(JonosijaHistoria jonosijaHistoria) {
        datastore.save(jonosijaHistoria);
    }
}
