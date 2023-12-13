package fi.vm.sade.valintalaskenta.laskenta.background;

import fi.vm.sade.valinta.dokumenttipalvelu.Dokumenttipalvelu;
import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteerihistoria;
import fi.vm.sade.valintalaskenta.laskenta.dao.JarjestyskriteerihistoriaDAO;
import fi.vm.sade.valintalaskenta.tulos.dao.util.JarjestyskriteeriKooderi;
import java.io.ByteArrayInputStream;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.Date;
import java.util.concurrent.TimeUnit;

import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

@Component
public class JarjestyskriteerihistoriaUploader {

  private static final Integer STORAGE_TIME_AROUND_6_YEARS =
      2192; // couple more in case of leap years

  private final Dokumenttipalvelu dokumenttipalvelu;

  private final JarjestyskriteerihistoriaDAO historiaDAO;

  public JarjestyskriteerihistoriaUploader(
      Dokumenttipalvelu dokumenttipalvelu, JarjestyskriteerihistoriaDAO historiaDAO) {
    this.dokumenttipalvelu = dokumenttipalvelu;
    this.historiaDAO = historiaDAO;
  }

  @Scheduled(initialDelay = 15, fixedRate = 20, timeUnit = TimeUnit.SECONDS)
  public void moveJarjestyskriteeriHistoriaFromDatabaseToS3() {
    historiaDAO.fetchOldest().forEach(this::uploadThenDelete);
  }

  private void uploadThenDelete(Jarjestyskriteerihistoria jarjestyskriteerihistoria) {
    upload(jarjestyskriteerihistoria);
    historiaDAO.delete(jarjestyskriteerihistoria.getId());
  }

  private void upload(Jarjestyskriteerihistoria jarjestyskriteerihistoria) {
    Jarjestyskriteerihistoria enkoodattu =
        JarjestyskriteeriKooderi.enkoodaa(jarjestyskriteerihistoria);
    dokumenttipalvelu.save(
        jarjestyskriteerihistoria.getId().toString(),
        jarjestyskriteerihistoria.getFilename(),
        Date.from(Instant.now().plus(STORAGE_TIME_AROUND_6_YEARS, ChronoUnit.DAYS)),
        Jarjestyskriteerihistoria.TAGS,
        "application/zip",
        new ByteArrayInputStream(enkoodattu.getHistoriaGzip()));
  }
}
