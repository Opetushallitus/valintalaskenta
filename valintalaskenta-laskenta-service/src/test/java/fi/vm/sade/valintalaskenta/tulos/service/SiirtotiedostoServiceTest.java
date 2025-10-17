package fi.vm.sade.valintalaskenta.tulos.service;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;

import fi.vm.sade.valintalaskenta.domain.dto.siirtotiedosto.DeletedValintapisteSiirtotiedostoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.siirtotiedosto.PistetietoSiirtotiedostoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.siirtotiedosto.PistetietoWrapperSiirtotiedostoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.siirtotiedosto.SiirtotiedostoResult;
import fi.vm.sade.valintalaskenta.domain.dto.valintapiste.Osallistumistieto;
import fi.vm.sade.valintalaskenta.domain.valintapiste.Valintapiste;
import fi.vm.sade.valintalaskenta.testing.AbstractIntegrationTest;
import fi.vm.sade.valintalaskenta.tulos.dao.ValintapisteDAO;
import fi.vm.sade.valintalaskenta.tulos.dao.repository.ValintapisteRepository;
import fi.vm.sade.valintalaskenta.tulos.ovara.SiirtotiedostoS3Client;
import java.time.LocalDateTime;
import java.util.List;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;

public class SiirtotiedostoServiceTest extends AbstractIntegrationTest {

  @Autowired private ValintapisteDAO valintapisteDAO;
  @Autowired private ValintapisteRepository valintapisteRepository;
  @Autowired private SiirtotiedostoService siirtotiedostoService;
  @Autowired private SiirtotiedostoS3Client s3Client;

  @Captor ArgumentCaptor<List<PistetietoWrapperSiirtotiedostoDTO>> dataCaptor;
  @Captor ArgumentCaptor<List<DeletedValintapisteSiirtotiedostoDTO>> poistettuCaptor;

  static final String HAKEMUS_OID = "hakemus-oid";
  static final Valintapiste DEFAULT_VALINTAPISTE =
      new Valintapiste(HAKEMUS_OID, "tunniste", "arvo", Osallistumistieto.OSALLISTUI, "Joku");
  static final LocalDateTime EPOCH = LocalDateTime.parse("1970-01-01T00:00:00");

  @BeforeEach
  public void resetMocks() {
    Mockito.reset(s3Client);
  }

  @Nested
  public class CreateSiirtotiedostotForValintakoeOsallistumiset {
    @Test
    public void doesNotCreateAnythingWhenThereAreNoMatchingEntries() {
      SiirtotiedostoResult result =
          siirtotiedostoService.createSiirtotiedostotForValintakoeOsallistumiset(
              EPOCH, LocalDateTime.now());

      assertThat(result).isEqualTo(new SiirtotiedostoResult(List.of(), 0));
      Mockito.verifyNoInteractions(s3Client);
    }
  }

  @Nested
  public class CreateSiirtotiedostotForValintalaskennanTulokset {
    @Test
    public void doesNotCreateAnythingWhenThereAreNoMatchingEntries() {
      SiirtotiedostoResult result =
          siirtotiedostoService.createSiirtotiedostotForValintalaskennanTulokset(
              EPOCH, LocalDateTime.now());

      assertThat(result).isEqualTo(new SiirtotiedostoResult(List.of(), 0));
      Mockito.verifyNoInteractions(s3Client);
    }
  }

  @Nested
  public class CreateSiirtotiedostotForValintapisteet {
    @Test
    public void doesNotCreateAnythingWhenThereAreNoMatchingEntries() {
      when(s3Client.getMaxHakemusCountInFile()).thenReturn(3);

      SiirtotiedostoResult result =
          siirtotiedostoService.createSiirtotiedostotForValintapisteet(EPOCH, LocalDateTime.now());

      assertThat(result).isEqualTo(new SiirtotiedostoResult(List.of(), 0));
      Mockito.verify(s3Client).getMaxHakemusCountInFile();
      Mockito.verifyNoMoreInteractions(s3Client);
    }

    @Test
    public void createsSiirtotiedostotInBatches() {
      when(s3Client.getMaxHakemusCountInFile()).thenReturn(3);
      when(s3Client.createSiirtotiedostoForTulosdata(any(), eq("pistetieto"), any(), eq(1)))
          .thenReturn("mocked-key");
      when(s3Client.createSiirtotiedostoForTulosdata(any(), eq("pistetieto"), any(), eq(2)))
          .thenReturn("mocked-key2");
      for (int i = 1; i < 6; i++) {
        valintapisteDAO.upsertValintapiste(DEFAULT_VALINTAPISTE.withHakemusOid("hakemus-" + i));
      }

      SiirtotiedostoResult result =
          siirtotiedostoService.createSiirtotiedostotForValintapisteet(EPOCH, LocalDateTime.now());

      assertThat(result)
          .isEqualTo(new SiirtotiedostoResult(List.of("mocked-key", "mocked-key2"), 5));

      Mockito.verify(s3Client)
          .createSiirtotiedostoForTulosdata(dataCaptor.capture(), any(), any(), eq(1));
      List<PistetietoWrapperSiirtotiedostoDTO> firstBatch = dataCaptor.getValue();
      assertThat(firstBatch.size()).isEqualTo(3);

      Mockito.verify(s3Client)
          .createSiirtotiedostoForTulosdata(dataCaptor.capture(), any(), any(), eq(2));
      List<PistetietoWrapperSiirtotiedostoDTO> secondBatch = dataCaptor.getValue();
      assertThat(secondBatch.size()).isEqualTo(2);

      List<String> hakemusOids =
          Stream.concat(firstBatch.stream(), secondBatch.stream())
              .map(PistetietoWrapperSiirtotiedostoDTO::hakemusOID)
              .toList();
      assertThat(hakemusOids)
          .containsExactlyInAnyOrder(
              "hakemus-1", "hakemus-2", "hakemus-3", "hakemus-4", "hakemus-5");

      Mockito.verify(s3Client).getMaxHakemusCountInFile();
      Mockito.verifyNoMoreInteractions(s3Client);
    }

    @Test
    public void readsAllPisteetForHakemukset() {
      when(s3Client.getMaxHakemusCountInFile()).thenReturn(3);
      when(s3Client.createSiirtotiedostoForTulosdata(any(), eq("pistetieto"), any(), eq(1)))
          .thenReturn("mocked-key");
      when(s3Client.createSiirtotiedostoForTulosdata(any(), eq("pistetieto"), any(), eq(2)))
          .thenReturn("mocked-key2");
      for (int i = 1; i < 6; i++) {
        valintapisteDAO.upsertValintapiste(DEFAULT_VALINTAPISTE.withTunniste("tunniste-" + i));
      }

      SiirtotiedostoResult result =
          siirtotiedostoService.createSiirtotiedostotForValintapisteet(EPOCH, LocalDateTime.now());

      assertThat(result)
          .isEqualTo(new SiirtotiedostoResult(List.of("mocked-key", "mocked-key2"), 5));

      Mockito.verify(s3Client)
          .createSiirtotiedostoForTulosdata(dataCaptor.capture(), any(), any(), eq(1));
      List<PistetietoWrapperSiirtotiedostoDTO> firstBatch = dataCaptor.getValue();
      assertThat(firstBatch.size()).isEqualTo(1);
      assertThat(firstBatch.get(0).hakemusOID()).isEqualTo(HAKEMUS_OID);

      Mockito.verify(s3Client)
          .createSiirtotiedostoForTulosdata(dataCaptor.capture(), any(), any(), eq(2));
      List<PistetietoWrapperSiirtotiedostoDTO> secondBatch = dataCaptor.getValue();
      assertThat(secondBatch.size()).isEqualTo(1);
      assertThat(secondBatch.get(0).hakemusOID()).isEqualTo(HAKEMUS_OID);

      List<String> hakemusOids =
          Stream.concat(firstBatch.stream(), secondBatch.stream())
              .flatMap(f -> f.pisteet().stream())
              .map(PistetietoSiirtotiedostoDTO::tunniste)
              .toList();
      assertThat(hakemusOids)
          .containsExactlyInAnyOrder(
              "tunniste-1", "tunniste-2", "tunniste-3", "tunniste-4", "tunniste-5");
    }

    @Test
    public void createsSiirtotiedostoForDeletedValintapisteetAfterChanges() {
      when(s3Client.getMaxHakemusCountInFile()).thenReturn(10);
      when(s3Client.createSiirtotiedostoForTulosdata(any(), eq("pistetieto"), any(), eq(1)))
          .thenReturn("mocked-key");
      when(s3Client.createSiirtotiedostoForTulosdata(any(), eq("pistetieto"), any(), eq(2)))
          .thenReturn("mocked-key2");
      for (int i = 1; i < 4; i++) {
        valintapisteDAO.upsertValintapiste(DEFAULT_VALINTAPISTE.withHakemusOid("hakemus-" + i));
      }
      valintapisteRepository.deleteByHakemusOid("hakemus-1");
      valintapisteRepository.deleteByHakemusOid("hakemus-2");

      SiirtotiedostoResult result =
          siirtotiedostoService.createSiirtotiedostotForValintapisteet(EPOCH, LocalDateTime.now());

      assertThat(result)
          .isEqualTo(new SiirtotiedostoResult(List.of("mocked-key", "mocked-key2"), 3));

      Mockito.verify(s3Client)
          .createSiirtotiedostoForTulosdata(dataCaptor.capture(), any(), any(), eq(1));
      List<PistetietoWrapperSiirtotiedostoDTO> firstBatch = dataCaptor.getValue();
      assertThat(firstBatch).singleElement().extracting("hakemusOID").isEqualTo("hakemus-3");

      Mockito.verify(s3Client)
          .createSiirtotiedostoForTulosdata(poistettuCaptor.capture(), any(), any(), eq(2));
      List<DeletedValintapisteSiirtotiedostoDTO> secondBatch = poistettuCaptor.getValue();
      assertThat(secondBatch)
          .containsExactlyInAnyOrder(
              new DeletedValintapisteSiirtotiedostoDTO("hakemus-1", true),
              new DeletedValintapisteSiirtotiedostoDTO("hakemus-2", true));
    }
  }
}
