package fi.vm.sade.valintalaskenta.tulos.service.impl;

import fi.vm.sade.valinta.dokumenttipalvelu.Dokumenttipalvelu;
import fi.vm.sade.valinta.dokumenttipalvelu.dto.ObjectEntity;
import fi.vm.sade.valintalaskenta.domain.valinta.Jarjestyskriteerihistoria;
import fi.vm.sade.valintalaskenta.tulos.dao.TulosJarjestyskriteerihistoriaDAO;
import fi.vm.sade.valintalaskenta.tulos.dao.util.JarjestyskriteeriKooderi;
import fi.vm.sade.valintalaskenta.tulos.service.JarjestyskriteerihistoriaService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import software.amazon.awssdk.services.s3.model.NoSuchKeyException;

import java.io.IOException;
import java.io.InputStream;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class JarjestyskriteerihistoriaServiceImplTest {

  private Dokumenttipalvelu dokumenttipalvelu = Mockito.mock(Dokumenttipalvelu.class);
  private TulosJarjestyskriteerihistoriaDAO dao = Mockito.mock(TulosJarjestyskriteerihistoriaDAO.class);
  private JarjestyskriteerihistoriaService service = new JarjestyskriteerihistoriaServiceImpl(dao, dokumenttipalvelu);

  private final InputStream input = Mockito.mock(InputStream.class);

  private final ObjectEntity entity = new ObjectEntity(input, "s", "s1", "s2", 1000L,
    Jarjestyskriteerihistoria.TAGS, Instant.now());;

  @BeforeEach
  public void init() throws IOException {
    Jarjestyskriteerihistoria historia = new Jarjestyskriteerihistoria();
    historia.setHistoria("");
    when(input.readAllBytes()).thenReturn(JarjestyskriteeriKooderi.enkoodaa(historia).getHistoriaGzip());
  }

  @Test
  public void hakeeHistoriatDokumenttipalvelusta() {
    String valintatapajonoOid = "jono1";
    String hakemusOid = "hakemus1";
    UUID id = UUID.randomUUID();
    when(dao.findByValintatapajonoAndHakemusOid(valintatapajonoOid, hakemusOid)).thenReturn(List.of(id));
    when(dokumenttipalvelu.composeKey(Jarjestyskriteerihistoria.TAGS, id.toString())).thenReturn("avain");
    when(dokumenttipalvelu.get("avain")).thenReturn(entity);
    assertEquals(1, service.findByValintatapajonoAndHakemusOid(valintatapajonoOid, hakemusOid).size());
  }

  @Test
  public void hakeeDokumenttipalvelustaPuuttuvanHistorianKannasta() {
    String valintatapajonoOid = "jono1";
    String hakemusOid = "hakemus1";
    UUID id = UUID.randomUUID();
    UUID idNotInS3 = UUID.randomUUID();
    when(dao.findByValintatapajonoAndHakemusOid(valintatapajonoOid, hakemusOid)).thenReturn(List.of(id, idNotInS3));
    when(dokumenttipalvelu.composeKey(Jarjestyskriteerihistoria.TAGS, id.toString())).thenReturn("avain");
    when(dokumenttipalvelu.composeKey(Jarjestyskriteerihistoria.TAGS, idNotInS3.toString())).thenReturn("avain2");
    when(dokumenttipalvelu.get("avain")).thenReturn(entity);
    when(dokumenttipalvelu.get("avain2")).thenThrow(Mockito.mock(NoSuchKeyException.class));
    when(dao.findById(List.of(idNotInS3))).thenReturn(List.of(new Jarjestyskriteerihistoria()));
    assertEquals(2, service.findByValintatapajonoAndHakemusOid(valintatapajonoOid, hakemusOid).size());
    verify(dao).findById(List.of(idNotInS3));
  }

  @Test
  public void heittaaPoikkeuksenJosTietoaEiLoydyKannastakaan() {
    String valintatapajonoOid = "jono1";
    String hakemusOid = "hakemus1";
    UUID idNotInS3 = UUID.randomUUID();
    when(dao.findByValintatapajonoAndHakemusOid(valintatapajonoOid, hakemusOid)).thenReturn(List.of(idNotInS3));
    when(dokumenttipalvelu.composeKey(Jarjestyskriteerihistoria.TAGS, idNotInS3.toString())).thenReturn("avain2");
    when(dokumenttipalvelu.get("avain2")).thenThrow(Mockito.mock(NoSuchKeyException.class));
    when(dao.findById(List.of(idNotInS3))).thenReturn(new ArrayList<>());
    assertThrows(RuntimeException.class,
      () -> service.findByValintatapajonoAndHakemusOid(valintatapajonoOid, hakemusOid));
  }
}
