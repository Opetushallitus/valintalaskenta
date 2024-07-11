package fi.vm.sade.valintalaskenta.laskenta.background;

import fi.vm.sade.valinta.dokumenttipalvelu.DocumentIdAlreadyExistsException;
import fi.vm.sade.valinta.dokumenttipalvelu.Dokumenttipalvelu;
import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteerihistoria;
import fi.vm.sade.valintalaskenta.laskenta.dao.JarjestyskriteerihistoriaDAO;
import fi.vm.sade.valintalaskenta.tulos.dao.util.JarjestyskriteeriKooderi;
import java.io.ByteArrayInputStream;
import java.util.List;
import java.util.concurrent.CompletionException;
import java.util.concurrent.TimeUnit;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

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

  @Scheduled(initialDelay = 5, fixedDelay = 10, timeUnit = TimeUnit.SECONDS)
  public void moveJarjestyskriteeriHistoriaFromDatabaseToS3() {
    long currentTime = System.currentTimeMillis();
    List<Jarjestyskriteerihistoria> historyIds = historiaDAO.fetchOldest();
    LOG.info("Fetching historias took " + (System.currentTimeMillis() - currentTime));
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
    long currentTime = System.currentTimeMillis();
    historiaDAO.delete(jarjestyskriteerihistoria.getId());
    LOG.info("Deleting history took " + (System.currentTimeMillis() - currentTime));
  }

  private void upload(Jarjestyskriteerihistoria jarjestyskriteerihistoria) {
    try {
      long currentTime = System.currentTimeMillis();
      Jarjestyskriteerihistoria enkoodattu =
          JarjestyskriteeriKooderi.enkoodaa(jarjestyskriteerihistoria);
      LOG.info("Enkoodaus kesti " + (System.currentTimeMillis() - currentTime));
      dokumenttipalvelu.save(
          jarjestyskriteerihistoria.getTunniste().toString(),
          jarjestyskriteerihistoria.getFilename(),
          Jarjestyskriteerihistoria.TAGS,
          "application/zip",
          new ByteArrayInputStream(enkoodattu.getHistoriaGzip()));
      LOG.info("UPLOAD took " + (System.currentTimeMillis() - currentTime));
    } catch (CompletionException | DocumentIdAlreadyExistsException e) {
      if (e instanceof DocumentIdAlreadyExistsException
          || e.getCause() instanceof DocumentIdAlreadyExistsException) {
        LOG.warn("Document already exists, skipping upload. {}", e.getMessage());
      } else {
        throw e;
      }
    }
  }

  private void update(Jarjestyskriteerihistoria jarjestyskriteerihistoria) {
    String key =
        dokumenttipalvelu.composeKey(
            Jarjestyskriteerihistoria.TAGS, jarjestyskriteerihistoria.getTunniste().toString());
    dokumenttipalvelu.moveToAnotherBucket(key, oldVersionBucketName);
  }
}
