package fi.vm.sade.valintalaskenta.laskenta.dao;

import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteerihistoria;
import org.bson.types.ObjectId;

/**
 * User: tommiha
 * Date: 8/9/13
 * Time: 10:37 AM
 */
public interface JarjestyskriteerihistoriaDAO {

    void create(Jarjestyskriteerihistoria jarjestyskriteerihistoria);

    void delete(ObjectId id);
}
