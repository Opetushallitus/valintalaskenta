package fi.vm.sade.valintalaskenta.laskenta.background;

import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import fi.vm.sade.valinta.dokumenttipalvelu.Dokumenttipalvelu;
import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteerihistoria;
import fi.vm.sade.valintalaskenta.laskenta.dao.JarjestyskriteerihistoriaDAO;
import java.io.InputStream;
import java.util.List;
import java.util.UUID;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

public class JarjestyskriteerihistoriaUploaderTest {

  private final Dokumenttipalvelu dokumenttipalvelu = Mockito.mock(Dokumenttipalvelu.class);

  private final JarjestyskriteerihistoriaDAO dao = Mockito.mock(JarjestyskriteerihistoriaDAO.class);

  private final JarjestyskriteerihistoriaUploader uploader =
      new JarjestyskriteerihistoriaUploader(dokumenttipalvelu, dao, "old");

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

  private Jarjestyskriteerihistoria createHistoria(boolean vanha) {
    Jarjestyskriteerihistoria historia = new Jarjestyskriteerihistoria();
    historia.setId(1L);
    historia.setLaskettuUudelleen(vanha);
    historia.setHistoria("historia");
    historia.setTunniste(UUID.randomUUID());
    return historia;
  }
}
