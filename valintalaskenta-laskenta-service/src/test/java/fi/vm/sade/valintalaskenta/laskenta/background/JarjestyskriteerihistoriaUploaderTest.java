package fi.vm.sade.valintalaskenta.laskenta.background;

import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import fi.vm.sade.valinta.dokumenttipalvelu.Dokumenttipalvelu;
import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteerihistoria;
import fi.vm.sade.valintalaskenta.laskenta.dao.JarjestyskriteerihistoriaDAO;
import java.io.InputStream;
import java.util.List;
import java.util.UUID;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.transaction.TransactionException;
import org.springframework.transaction.support.TransactionCallback;
import org.springframework.transaction.support.TransactionOperations;
import software.amazon.awssdk.services.s3.model.NoSuchKeyException;

public class JarjestyskriteerihistoriaUploaderTest {

  private final Dokumenttipalvelu dokumenttipalvelu = Mockito.mock(Dokumenttipalvelu.class);

  private final JarjestyskriteerihistoriaDAO dao = Mockito.mock(JarjestyskriteerihistoriaDAO.class);

  private final JarjestyskriteerihistoriaUploader uploader =
      new JarjestyskriteerihistoriaUploader(
          dokumenttipalvelu,
          dao,
          new TransactionOperations() {
            @Override
            public <T> T execute(TransactionCallback<T> action) throws TransactionException {
              return action.doInTransaction(null);
            }
          },
          "old");

  @BeforeEach
  public void init() {
    when(dokumenttipalvelu.composeKey(any(), any())).thenCallRealMethod();
  }

  @Test
  public void uploadsHistoriaAndThenDeletes() {
    when(dao.fetchOldest()).thenReturn(List.of(createHistoria(false)));
    uploader.moveJarjestyskriteeriHistoriaFromDatabaseToS3();
    verify(dokumenttipalvelu)
        .putObject(anyString(), anyString(), anyString(), any(InputStream.class));
    verify(dao).delete(any(Long.class));
  }

  @Test
  public void updatesHistoriaAndThenDeletes() {
    when(dao.fetchOldest()).thenReturn(List.of(createHistoria(true)));
    uploader.moveJarjestyskriteeriHistoriaFromDatabaseToS3();
    verify(dokumenttipalvelu).moveToAnotherBucket(anyString(), anyString());
    verify(dao).delete(any(Long.class));
  }

  @Test
  public void deletesHistoriaWhenUpdateOperationFailsToNoSuchKeyException() {
    when(dao.fetchOldest()).thenReturn(List.of(createHistoria(true)));
    doThrow(Mockito.mock(NoSuchKeyException.class))
        .when(dokumenttipalvelu)
        .moveToAnotherBucket(anyString(), anyString());
    uploader.moveJarjestyskriteeriHistoriaFromDatabaseToS3();
    verify(dao).delete(any(Long.class));
  }

  @Test
  public void doesNotDeleteHistoriaWhenUpdateOperationFailsToSomeOtherError() {
    when(dao.fetchOldest()).thenReturn(List.of(createHistoria(true)));
    doThrow(mock(NullPointerException.class))
        .when(dokumenttipalvelu)
        .moveToAnotherBucket(anyString(), anyString());
    try {
      uploader.moveJarjestyskriteeriHistoriaFromDatabaseToS3();
    } catch (Exception ignored) {

    }
    verify(dao, never()).delete(any(Long.class));
  }

  private Jarjestyskriteerihistoria createHistoria(boolean vanha) {
    Jarjestyskriteerihistoria historia = new Jarjestyskriteerihistoria();
    historia.setId(1L);
    historia.setLaskettuUudelleen(vanha);
    historia.setHistoria("historia");
    historia.setTunniste(UUID.randomUUID());
    return historia;
  }
}
