package fi.vm.sade.valintalaskenta.laskenta.dao.impl;

import com.google.code.morphia.Datastore;
import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteerihistoria;
import fi.vm.sade.valintalaskenta.laskenta.dao.JarjestyskriteerihistoriaDAO;
import org.bson.types.ObjectId;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

/**
 * User: tommiha
 * Date: 8/9/13
 * Time: 10:38 AM
 */
@Repository("jonosijaHistoriaDAO")
public class JarjestyskriteerihistoriaDAOImpl implements JarjestyskriteerihistoriaDAO {

    @Autowired
    private Datastore datastore;

    @Override
    public void create(Jarjestyskriteerihistoria jarjestyskriteerihistoria) {
        datastore.save(jarjestyskriteerihistoria);
    }

    @Override
    public void delete(ObjectId id) {
        datastore.delete(Jarjestyskriteerihistoria.class, id);
    }

    @Override
    public Jarjestyskriteerihistoria hae(ObjectId id) {
        return datastore.find(Jarjestyskriteerihistoria.class).field("_id").equal(id).get();
    }
}
