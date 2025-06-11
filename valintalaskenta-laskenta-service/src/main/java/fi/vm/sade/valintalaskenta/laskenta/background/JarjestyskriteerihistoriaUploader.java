package fi.vm.sade.valintalaskenta.laskenta.background;

import fi.vm.sade.valinta.dokumenttipalvelu.Dokumenttipalvelu;
import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteerihistoria;
import fi.vm.sade.valintalaskenta.laskenta.dao.JarjestyskriteerihistoriaDAO;
import fi.vm.sade.valintalaskenta.tulos.dao.util.JarjestyskriteeriKooderi;
import java.io.ByteArrayInputStream;
import java.util.List;
import java.util.concurrent.CompletionException;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import org.flywaydb.core.internal.util.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import org.springframework.transaction.support.TransactionOperations;
import software.amazon.awssdk.services.s3.model.NoSuchKeyException;

@Component
public class JarjestyskriteerihistoriaUploader {

  private static final Logger LOG =
      LoggerFactory.getLogger(JarjestyskriteerihistoriaUploader.class);

  private final Dokumenttipalvelu dokumenttipalvelu;

  private final JarjestyskriteerihistoriaDAO historiaDAO;

  private final String oldVersionBucketName;

  private final Executor executor;

  private final TransactionOperations transactionOperations;

  private boolean isMoving = false;

  public JarjestyskriteerihistoriaUploader(
      Dokumenttipalvelu dokumenttipalvelu,
      JarjestyskriteerihistoriaDAO historiaDAO,
      TransactionOperations transactionOperations,
      @Value("${aws.bucket.oldversionname}") final String oldBucketName) {
    this.dokumenttipalvelu = dokumenttipalvelu;
    this.historiaDAO = historiaDAO;
    this.oldVersionBucketName = oldBucketName;
    this.transactionOperations = transactionOperations;
    this.executor = Executors.newSingleThreadExecutor();
  }

  @Scheduled(initialDelay = 1500, fixedDelay = 10000, timeUnit = TimeUnit.MILLISECONDS)
  public void runJarjestysKriteeriHistoriaUploader() {
    if (this.isMoving) return;
    this.isMoving = true;
    this.executor.execute(
        () -> {
          try {
            this.transactionOperations.executeWithoutResult(
                ts -> {
                  this.moveJarjestyskriteeriHistoriaFromDatabaseToS3();
                });
          } finally {
            this.isMoving = false;
          }
        });
  }

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
