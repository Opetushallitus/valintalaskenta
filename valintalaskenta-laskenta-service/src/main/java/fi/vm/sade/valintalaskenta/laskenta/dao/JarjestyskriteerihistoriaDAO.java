package fi.vm.sade.valintalaskenta.laskenta.dao;

import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteerihistoria;
import org.bson.types.ObjectId;

public interface JarjestyskriteerihistoriaDAO {

    void create(Jarjestyskriteerihistoria jarjestyskriteerihistoria);

    void delete(ObjectId id);

    Jarjestyskriteerihistoria hae(ObjectId id);
}
