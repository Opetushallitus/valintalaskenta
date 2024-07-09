package fi.vm.sade.valintalaskenta.tulos.service.impl;

import com.google.common.collect.Lists;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import fi.vm.sade.valintalaskenta.domain.dto.siirtotiedosto.ValintakoeOsallistuminenSiirtotiedostoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.siirtotiedosto.ValintatietoValinnanvaiheSiirtotiedostoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.ValintakoeOsallistuminenDTO;
import fi.vm.sade.valintalaskenta.tulos.SiirtotiedostoS3Client;
import fi.vm.sade.valintalaskenta.tulos.dao.TulosValinnanvaiheDAO;
import fi.vm.sade.valintalaskenta.tulos.dao.TulosValintakoeOsallistuminenDAO;
import fi.vm.sade.valintalaskenta.tulos.mapping.ValintalaskentaModelMapper;
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
  private final ValintalaskentaTulosService valintalaskentaTulosService;
  private final ValintalaskentaModelMapper modelMapper;
  private final ValintatulosConverter valintatulosConverter;

  private final SiirtotiedostoS3Client siirtotiedostoS3Client;

  @Autowired
  public SiirtotiedostoServiceImpl(
      final TulosValintakoeOsallistuminenDAO tulosValintakoeOsallistuminenDAO,
      final TulosValinnanvaiheDAO tulosValinnanvaiheDAO,
      final ValintalaskentaTulosService tulosService,
      final ValintalaskentaModelMapper modelMapper,
      final ValintatulosConverter valintatulosConverter,
      final SiirtotiedostoS3Client siirtotiedostoS3Client) {
    this.tulosValintakoeOsallistuminenDAO = tulosValintakoeOsallistuminenDAO;
    this.tulosValinnanvaiheDAO = tulosValinnanvaiheDAO;
    this.valintalaskentaTulosService = tulosService;
    this.modelMapper = modelMapper;
    this.valintatulosConverter = valintatulosConverter;
    this.siirtotiedostoS3Client = siirtotiedostoS3Client;
  }

  @Override
  public String createSiirtotiedostotForValintakoeOsallistumiset(
      LocalDateTime startDatetime, LocalDateTime endDatatime) {
    String opId = UUID.randomUUID().toString();
    List<String> hakemusOids =
        tulosValintakoeOsallistuminenDAO.readNewOrModifiedHakemusOids(startDatetime, endDatatime);
    List<List<String>> partitions =
        Lists.partition(hakemusOids, siirtotiedostoS3Client.getMaxHakemusCountInFile());
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
    return resultJson(siirtotiedostoKeys, hakemusOids.size());
  }

  @Override
  public String createSiirtotiedostotForValintalaskennanTulokset(
      LocalDateTime startDatetime, LocalDateTime endDatatime) {
    String opId = UUID.randomUUID().toString();
    List<String> hakukohdeOids =
        tulosValinnanvaiheDAO.readNewOrModifiedHakukohdeOids(startDatetime, endDatatime);
    List<List<String>> partitions =
        Lists.partition(hakukohdeOids, siirtotiedostoS3Client.getMaxHakukohdeCountInFile());
    List<String> siirtotiedostoKeys = new ArrayList<>();
    for (List<String> hakekohdeOidChunk : partitions) {
      List<List<ValintatietoValinnanvaiheSiirtotiedostoDTO>> tulokset = new ArrayList<>();
      for (String hakukohdeOid : hakekohdeOidChunk) {

        tulokset.add(
            valintalaskentaTulosService.haeValinnanvaiheetHakukohteelleForSiirtotiedosto(
                hakukohdeOid));
      }
      siirtotiedostoKeys.add(
          siirtotiedostoS3Client.createSiirtotiedostoForTulosdata(
              tulokset.stream().flatMap(List::stream).collect(Collectors.toList()),
              "valintalaskennan_tulos",
              opId,
              siirtotiedostoKeys.size() + 1));
    }
    LOGGER.info(
        "Kirjoitettiin yhteensä {} valintalaskennan tulosta {} siirtotiedostoon.",
        hakukohdeOids.size(),
        siirtotiedostoKeys.size());
    return resultJson(siirtotiedostoKeys, hakukohdeOids.size());
  }

  private String resultJson(List<String> siirtotiedostoKeys, int itemCount) {
    JsonArray keyJson = new JsonArray();
    siirtotiedostoKeys.forEach(key -> keyJson.add(key));
    JsonObject result = new JsonObject();
    result.add("keys", keyJson);
    result.addProperty("total", itemCount);
    result.addProperty("success", true);
    return result.toString();
  }
}
