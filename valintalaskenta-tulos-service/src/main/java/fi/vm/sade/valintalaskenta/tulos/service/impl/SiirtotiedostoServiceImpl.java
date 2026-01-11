package fi.vm.sade.valintalaskenta.tulos.service.impl;

import com.google.common.collect.Lists;
import fi.vm.sade.valintalaskenta.domain.dto.siirtotiedosto.DeletedValintapisteSiirtotiedostoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.siirtotiedosto.SiirtotiedostoResult;
import fi.vm.sade.valintalaskenta.domain.dto.siirtotiedosto.ValintakoeOsallistuminenSiirtotiedostoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.siirtotiedosto.ValintatietoValinnanvaiheSiirtotiedostoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.ValintakoeOsallistuminenDTO;
import fi.vm.sade.valintalaskenta.domain.valintapiste.ValintapisteWithLastModified;
import fi.vm.sade.valintalaskenta.tulos.dao.TulosValinnanvaiheDAO;
import fi.vm.sade.valintalaskenta.tulos.dao.TulosValintakoeOsallistuminenDAO;
import fi.vm.sade.valintalaskenta.tulos.dao.ValintapisteDAO;
import fi.vm.sade.valintalaskenta.tulos.mapping.ValintalaskentaModelMapper;
import fi.vm.sade.valintalaskenta.tulos.ovara.SiirtotiedostoS3Client;
import fi.vm.sade.valintalaskenta.tulos.service.SiirtotiedostoService;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import fi.vm.sade.valintalaskenta.tulos.service.impl.converters.ValintatulosConverter;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Service;

@Service
@ConditionalOnProperty("valintalaskenta.enable-siirtotiedosto-service")
public class SiirtotiedostoServiceImpl implements SiirtotiedostoService {
  private static final Logger LOGGER = LoggerFactory.getLogger(SiirtotiedostoServiceImpl.class);

  private final TulosValintakoeOsallistuminenDAO tulosValintakoeOsallistuminenDAO;
  private final TulosValinnanvaiheDAO tulosValinnanvaiheDAO;
  private final ValintapisteDAO valintapisteDAO;
  private final ValintalaskentaTulosService valintalaskentaTulosService;
  private final ValintalaskentaModelMapper modelMapper;
  private final ValintatulosConverter valintatulosConverter;

  private final SiirtotiedostoS3Client siirtotiedostoS3Client;

  @Autowired
  public SiirtotiedostoServiceImpl(
      final TulosValintakoeOsallistuminenDAO tulosValintakoeOsallistuminenDAO,
      final TulosValinnanvaiheDAO tulosValinnanvaiheDAO,
      final ValintapisteDAO valintapisteDAO,
      final ValintalaskentaTulosService tulosService,
      final ValintalaskentaModelMapper modelMapper,
      final ValintatulosConverter valintatulosConverter,
      final SiirtotiedostoS3Client siirtotiedostoS3Client) {
    this.tulosValintakoeOsallistuminenDAO = tulosValintakoeOsallistuminenDAO;
    this.tulosValinnanvaiheDAO = tulosValinnanvaiheDAO;
    this.valintapisteDAO = valintapisteDAO;
    this.valintalaskentaTulosService = tulosService;
    this.modelMapper = modelMapper;
    this.valintatulosConverter = valintatulosConverter;
    this.siirtotiedostoS3Client = siirtotiedostoS3Client;
  }

  @Override
  public SiirtotiedostoResult createSiirtotiedostotForValintakoeOsallistumiset(
      LocalDateTime startDatetime, LocalDateTime endDatatime) {
    String opId = UUID.randomUUID().toString();
    List<String> hakemusOids =
        tulosValintakoeOsallistuminenDAO.readNewOrModifiedHakemusOids(startDatetime, endDatatime);
    List<List<String>> partitions =
        hakemusOids.isEmpty()
            ? List.of()
            : Lists.partition(hakemusOids, siirtotiedostoS3Client.getMaxHakemusCountInFile());
    List<String> siirtotiedostoKeys = new ArrayList<>();
    for (List<String> hakemusOidChunk : partitions) {
      List<ValintakoeOsallistuminenDTO> osallistumiset = new ArrayList<>();
      for (String hakemusOid : hakemusOidChunk) {
        osallistumiset.add(
            modelMapper.map(
                valintalaskentaTulosService.haeValintakoeOsallistumiset(hakemusOid),
                ValintakoeOsallistuminenDTO.class));
      }
      List<ValintakoeOsallistuminenSiirtotiedostoDTO> osallistumisetAsSiirtotiedostoData =
          valintatulosConverter.convertValintakoeOsallistuminenListForSiirtotiedosto(
              osallistumiset);
      siirtotiedostoKeys.add(
          siirtotiedostoS3Client.createSiirtotiedostoForTulosdata(
              osallistumisetAsSiirtotiedostoData,
              "valintakoe_osallistuminen",
              opId,
              siirtotiedostoKeys.size() + 1));
    }
    LOGGER.info(
        "Kirjoitettiin yhteensä {} hakemuksen valintakoeosallistumiset {} siirtotiedostoon.",
        hakemusOids.size(),
        siirtotiedostoKeys.size());
    return new SiirtotiedostoResult(siirtotiedostoKeys, hakemusOids.size());
  }

  @Override
  public SiirtotiedostoResult createSiirtotiedostotForValintalaskennanTulokset(
      LocalDateTime startDatetime, LocalDateTime endDatatime) {
    String opId = UUID.randomUUID().toString();
    List<String> valinnanvaiheOids =
        tulosValinnanvaiheDAO.readNewOrModifiedValinnanvaiheOids(startDatetime, endDatatime);
    List<List<String>> partitions =
        valinnanvaiheOids.isEmpty()
            ? List.of()
            : Lists.partition(
                valinnanvaiheOids, siirtotiedostoS3Client.getMaxValinnanvaiheCountInFile());
    List<String> siirtotiedostoKeys = new ArrayList<>();
    for (List<String> valinnanvaiheOidChunk : partitions) {
      List<List<ValintatietoValinnanvaiheSiirtotiedostoDTO>> tulokset = new ArrayList<>();
      tulokset.add(
          valintalaskentaTulosService.haeValinnanvaiheetForSiirtotiedosto(valinnanvaiheOidChunk));
      siirtotiedostoKeys.add(
          siirtotiedostoS3Client.createSiirtotiedostoForTulosdata(
              tulokset.stream().flatMap(List::stream).collect(Collectors.toList()),
              "valintalaskennan_tulos",
              opId,
              siirtotiedostoKeys.size() + 1));
    }
    LOGGER.info(
        "Kirjoitettiin yhteensä {} valintalaskennan tulosta {} siirtotiedostoon.",
        valinnanvaiheOids.size(),
        siirtotiedostoKeys.size());
    return new SiirtotiedostoResult(siirtotiedostoKeys, valinnanvaiheOids.size());
  }

  @Override
  public SiirtotiedostoResult createSiirtotiedostotForValintapisteet(
      LocalDateTime start, LocalDateTime end) {
    int batchSize = siirtotiedostoS3Client.getMaxHakemusCountInFile();
    String opId = UUID.randomUUID().toString();
    int offset = 0;
    List<String> siirtotiedostoKeys = new ArrayList<>();
    List<ValintapisteWithLastModified> results =
        valintapisteDAO.findValintapisteBulkByTimerange(start, end, batchSize, offset);

    while (!results.isEmpty()) {
      LOGGER.info("Luodaan siirtotiedosto valintapisteille, offset {}", offset);

      siirtotiedostoKeys.add(
          siirtotiedostoS3Client.createSiirtotiedostoForTulosdata(
              valintatulosConverter.convertPistetiedotForSiirtotiedosto(results),
              "pistetieto",
              opId,
              siirtotiedostoKeys.size() + 1));

      offset += results.size();
      results = valintapisteDAO.findValintapisteBulkByTimerange(start, end, batchSize, offset);
    }

    List<DeletedValintapisteSiirtotiedostoDTO> deleted =
        valintapisteDAO.findDeleted(start, end).stream()
            .map(d -> new DeletedValintapisteSiirtotiedostoDTO(d, true))
            .toList();
    if (!deleted.isEmpty()) {
      LOGGER.info("Luodaan siirtotiedosto poistetuille valintapisteille, offset {}", offset);
      siirtotiedostoKeys.add(
          siirtotiedostoS3Client.createSiirtotiedostoForTulosdata(
              deleted, "pistetieto", opId, siirtotiedostoKeys.size() + 1));
      offset += deleted.size();
    }

    LOGGER.info(
        "Kirjoitettiin yhteensä {} valintapistettä {} siirtotiedostoon.",
        offset,
        siirtotiedostoKeys.size());
    return new SiirtotiedostoResult(siirtotiedostoKeys, offset);
  }
}
