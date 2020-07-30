package fi.vm.sade.valintalaskenta.laskenta.dao.impl;

import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteerihistoria;
import fi.vm.sade.valintalaskenta.laskenta.dao.JarjestyskriteerihistoriaDAO;
import fi.vm.sade.valintalaskenta.tulos.dao.util.JarjestyskriteeriKooderi;
import java.util.stream.Stream;
import org.bson.types.ObjectId;
import org.mongodb.morphia.Datastore;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

@Repository("jonosijaHistoriaDAO")
public class JarjestyskriteerihistoriaDAOImpl implements JarjestyskriteerihistoriaDAO {
  private static final Logger LOG = LoggerFactory.getLogger(JarjestyskriteerihistoriaDAOImpl.class);
  @Autowired private Datastore datastore;

  @Override
  public void create(Jarjestyskriteerihistoria jarjestyskriteerihistoria) {
    datastore.save(JarjestyskriteeriKooderi.enkoodaa(jarjestyskriteerihistoria));
  }

  @Override
  public void delete(ObjectId id) {
    datastore.delete(Jarjestyskriteerihistoria.class, id);
  }

  @Override
  public Jarjestyskriteerihistoria hae(ObjectId id) {
    Jarjestyskriteerihistoria h =
        datastore.find(Jarjestyskriteerihistoria.class).field("_id").equal(id).get();
    Stream.of(h).filter(JarjestyskriteeriKooderi::tarvitseekoEnkoodata).forEach(this::create);
    return JarjestyskriteeriKooderi.dekoodaa(h);
  }
}
