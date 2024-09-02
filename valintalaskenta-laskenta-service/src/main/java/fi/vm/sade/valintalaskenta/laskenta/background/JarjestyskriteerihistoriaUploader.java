package fi.vm.sade.valintalaskenta.laskenta.background;

import fi.vm.sade.valinta.dokumenttipalvelu.Dokumenttipalvelu;
import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteerihistoria;
import fi.vm.sade.valintalaskenta.laskenta.dao.JarjestyskriteerihistoriaDAO;
import fi.vm.sade.valintalaskenta.tulos.dao.util.JarjestyskriteeriKooderi;
import java.io.ByteArrayInputStream;
import java.util.List;
import java.util.concurrent.CompletionException;
import java.util.concurrent.TimeUnit;
import org.flywaydb.core.internal.util.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import software.amazon.awssdk.services.s3.model.NoSuchKeyException;

@Component
public class JarjestyskriteerihistoriaUploader {

  private static final Logger LOG =
      LoggerFactory.getLogger(JarjestyskriteerihistoriaUploader.class);

  private final Dokumenttipalvelu dokumenttipalvelu;

  private final JarjestyskriteerihistoriaDAO historiaDAO;

  private final String oldVersionBucketName;

  public JarjestyskriteerihistoriaUploader(
      Dokumenttipalvelu dokumenttipalvelu,
      JarjestyskriteerihistoriaDAO historiaDAO,
      @Value("${aws.bucket.oldversionname}") final String oldBucketName) {
    this.dokumenttipalvelu = dokumenttipalvelu;
    this.historiaDAO = historiaDAO;
    this.oldVersionBucketName = oldBucketName;
  }

  @Scheduled(initialDelay = 15, fixedDelay = 10, timeUnit = TimeUnit.SECONDS)
  public void moveJarjestyskriteeriHistoriaFromDatabaseToS3() {
    List<Jarjestyskriteerihistoria> historyIds = historiaDAO.fetchOldest();
    if (historyIds.isEmpty()) {
      LOG.debug("Jarjestyskriteerihistorioita ei löytynyt kannasta");
    } else {
      historyIds.forEach(this::uploadThenDelete);
    }
  }

  private void uploadThenDelete(Jarjestyskriteerihistoria jarjestyskriteerihistoria) {
    if (jarjestyskriteerihistoria.isLaskettuUudelleen()) {
      update(jarjestyskriteerihistoria);
    } else {
      upload(jarjestyskriteerihistoria);
    }
    historiaDAO.delete(jarjestyskriteerihistoria.getId());
  }

  private void upload(Jarjestyskriteerihistoria jarjestyskriteerihistoria) {
    Jarjestyskriteerihistoria enkoodattu =
        JarjestyskriteeriKooderi.enkoodaa(jarjestyskriteerihistoria);
    String key =
        dokumenttipalvelu.composeKey(
            Jarjestyskriteerihistoria.TAGS, jarjestyskriteerihistoria.getTunniste().toString());
    dokumenttipalvelu.putObject(
        key,
        jarjestyskriteerihistoria.getFilename(),
        "application/zip",
        new ByteArrayInputStream(enkoodattu.getHistoriaGzip()));
  }

  private void update(Jarjestyskriteerihistoria jarjestyskriteerihistoria) {
    String key =
        dokumenttipalvelu.composeKey(
            Jarjestyskriteerihistoria.TAGS, jarjestyskriteerihistoria.getTunniste().toString());
    try {
      dokumenttipalvelu.moveToAnotherBucket(key, oldVersionBucketName);
    } catch (NoSuchKeyException | CompletionException e) {
      if (ExceptionUtils.getRootCause(e) instanceof NoSuchKeyException) {
        LOG.warn("Siirrettävää järjestyskriteehistoriaa ei löytynyt avaimella {}", key, e);
      } else {
        throw e;
      }
    }
  }
}
