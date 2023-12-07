package fi.vm.sade.valintalaskenta.laskenta.dao.impl;

import fi.vm.sade.valinta.dokumenttipalvelu.Dokumenttipalvelu;
import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteerihistoria;
import fi.vm.sade.valintalaskenta.laskenta.dao.JarjestyskriteerihistoriaDAO;
import fi.vm.sade.valintalaskenta.tulos.dao.util.JarjestyskriteeriKooderi;
import java.io.ByteArrayInputStream;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.Arrays;
import java.util.Date;
import java.util.stream.Stream;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Repository;

@Repository
public class JarjestyskriteerihistoriaDAOImpl implements JarjestyskriteerihistoriaDAO {

  private static final Logger LOG = LoggerFactory.getLogger(JarjestyskriteerihistoriaDAOImpl.class);

  private static final Integer STORAGE_TIME_AROUND_6_YEARS =
      2192; // couple more in case of leap years
  private final Dokumenttipalvelu dokumenttipalvelu;

  public JarjestyskriteerihistoriaDAOImpl(Dokumenttipalvelu dokumenttipalvelu) {
    this.dokumenttipalvelu = dokumenttipalvelu;
  }

  @Override
  public void create(Jarjestyskriteerihistoria jarjestyskriteerihistoria) {
    Jarjestyskriteerihistoria enkoodattu =
        JarjestyskriteeriKooderi.enkoodaa(jarjestyskriteerihistoria);
    dokumenttipalvelu.save(
        jarjestyskriteerihistoria.getId(),
        jarjestyskriteerihistoria.getFilename(),
        Date.from(Instant.now().plus(STORAGE_TIME_AROUND_6_YEARS, ChronoUnit.DAYS)),
        Arrays.asList("valintalaskenta", "jarjestyskriteerihistoria"),
        "application/zip",
        new ByteArrayInputStream(enkoodattu.getHistoriaGzip()));
  }

  @Override
  public void delete(String id) {
    // datastore.delete(Jarjestyskriteerihistoria.class, id);
  }

  @Override
  public Jarjestyskriteerihistoria hae(String id) {
    Jarjestyskriteerihistoria h = new Jarjestyskriteerihistoria();
    // datastore.find(Jarjestyskriteerihistoria.class).field("_id").equal(id).get();
    Stream.of(h).filter(JarjestyskriteeriKooderi::tarvitseekoEnkoodata).forEach(this::create);
    return JarjestyskriteeriKooderi.dekoodaa(h);
  }
}
