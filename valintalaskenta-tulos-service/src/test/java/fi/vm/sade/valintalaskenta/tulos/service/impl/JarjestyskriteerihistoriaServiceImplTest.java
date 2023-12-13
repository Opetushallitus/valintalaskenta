package fi.vm.sade.valintalaskenta.tulos.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import fi.vm.sade.valinta.dokumenttipalvelu.Dokumenttipalvelu;
import fi.vm.sade.valinta.dokumenttipalvelu.dto.ObjectEntity;
import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteerihistoria;
import fi.vm.sade.valintalaskenta.tulos.dao.TulosJarjestyskriteerihistoriaDAO;
import fi.vm.sade.valintalaskenta.tulos.dao.util.JarjestyskriteeriKooderi;
import fi.vm.sade.valintalaskenta.tulos.service.JarjestyskriteerihistoriaService;
import java.io.IOException;
import java.io.InputStream;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import software.amazon.awssdk.services.s3.model.NoSuchKeyException;

public class JarjestyskriteerihistoriaServiceImplTest {

  private final Dokumenttipalvelu dokumenttipalvelu = Mockito.mock(Dokumenttipalvelu.class);
  private final TulosJarjestyskriteerihistoriaDAO dao =
      Mockito.mock(TulosJarjestyskriteerihistoriaDAO.class);
  private final JarjestyskriteerihistoriaService service =
      new JarjestyskriteerihistoriaServiceImpl(dao, dokumenttipalvelu);

  private final InputStream input = Mockito.mock(InputStream.class);

  private final ObjectEntity entity =
      new ObjectEntity(
          input, "s", "s1", "s2", 1000L, Jarjestyskriteerihistoria.TAGS, Instant.now());
  ;
  private static final String JONO_OID = "jono1";
  private static final String HAKEMUS_OID = "hakemus1";

  private static final UUID ID = UUID.randomUUID(), ID_NOT_IN_S3 = UUID.randomUUID();

  @BeforeEach
  public void init() throws IOException {
    Jarjestyskriteerihistoria historia = new Jarjestyskriteerihistoria();
    historia.setHistoria("");
    when(input.readAllBytes())
        .thenReturn(JarjestyskriteeriKooderi.enkoodaa(historia).getHistoriaGzip());
  }

  @Test
  public void hakeeHistoriatDokumenttipalvelusta() {
    when(dao.findByValintatapajonoAndHakemusOid(JONO_OID, HAKEMUS_OID)).thenReturn(List.of(ID));
    when(dokumenttipalvelu.composeKey(Jarjestyskriteerihistoria.TAGS, ID.toString()))
        .thenReturn("avain");
    when(dokumenttipalvelu.get("avain")).thenReturn(entity);
    assertEquals(1, service.findByValintatapajonoAndHakemusOid(JONO_OID, HAKEMUS_OID).size());
  }

  @Test
  public void hakeeDokumenttipalvelustaPuuttuvanHistorianKannasta() {
    when(dao.findByValintatapajonoAndHakemusOid(JONO_OID, HAKEMUS_OID))
        .thenReturn(List.of(ID, ID_NOT_IN_S3));
    when(dokumenttipalvelu.composeKey(Jarjestyskriteerihistoria.TAGS, ID.toString()))
        .thenReturn("avain");
    when(dokumenttipalvelu.composeKey(Jarjestyskriteerihistoria.TAGS, ID_NOT_IN_S3.toString()))
        .thenReturn("avain2");
    when(dokumenttipalvelu.get("avain")).thenReturn(entity);
    when(dokumenttipalvelu.get("avain2")).thenThrow(Mockito.mock(NoSuchKeyException.class));
    when(dao.findById(List.of(ID_NOT_IN_S3))).thenReturn(List.of(new Jarjestyskriteerihistoria()));
    assertEquals(2, service.findByValintatapajonoAndHakemusOid(JONO_OID, HAKEMUS_OID).size());
    verify(dao).findById(List.of(ID_NOT_IN_S3));
  }

  @Test
  public void heittaaPoikkeuksenJosTietoaEiLoydyKannastakaan() {
    when(dao.findByValintatapajonoAndHakemusOid(JONO_OID, HAKEMUS_OID))
        .thenReturn(List.of(ID_NOT_IN_S3));
    when(dokumenttipalvelu.composeKey(Jarjestyskriteerihistoria.TAGS, ID_NOT_IN_S3.toString()))
        .thenReturn("avain2");
    when(dokumenttipalvelu.get("avain2")).thenThrow(Mockito.mock(NoSuchKeyException.class));
    when(dao.findById(List.of(ID_NOT_IN_S3))).thenReturn(new ArrayList<>());
    assertThrows(
        RuntimeException.class,
        () -> service.findByValintatapajonoAndHakemusOid(JONO_OID, HAKEMUS_OID));
  }
}
