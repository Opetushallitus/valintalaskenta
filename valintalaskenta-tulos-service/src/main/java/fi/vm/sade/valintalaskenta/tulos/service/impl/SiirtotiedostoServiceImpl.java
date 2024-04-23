package fi.vm.sade.valintalaskenta.tulos.service.impl;

import com.google.common.collect.Lists;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.ValintatietoValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import fi.vm.sade.valintalaskenta.tulos.SiirtotiedostoS3Client;
import fi.vm.sade.valintalaskenta.tulos.dao.TulosValinnanvaiheDAO;
import fi.vm.sade.valintalaskenta.tulos.dao.TulosValintakoeOsallistuminenDAO;
import fi.vm.sade.valintalaskenta.tulos.service.SiirtotiedostoService;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class SiirtotiedostoServiceImpl implements SiirtotiedostoService {
  private static final Logger LOGGER = LoggerFactory.getLogger(SiirtotiedostoServiceImpl.class);

  private final TulosValintakoeOsallistuminenDAO tulosValintakoeOsallistuminenDAO;
  private final TulosValinnanvaiheDAO tulosValinnanvaiheDAO;
  private final ValintalaskentaTulosService valintalaskentaTulosService;

  private final SiirtotiedostoS3Client siirtotiedostoS3Client;

  @Autowired
  public SiirtotiedostoServiceImpl(
      final TulosValintakoeOsallistuminenDAO tulosValintakoeOsallistuminenDAO,
      final TulosValinnanvaiheDAO tulosValinnanvaiheDAO,
      final ValintalaskentaTulosService tulosService,
      final SiirtotiedostoS3Client siirtotiedostoS3Client) {
    this.tulosValintakoeOsallistuminenDAO = tulosValintakoeOsallistuminenDAO;
    this.tulosValinnanvaiheDAO = tulosValinnanvaiheDAO;
    this.valintalaskentaTulosService = tulosService;
    this.siirtotiedostoS3Client = siirtotiedostoS3Client;
  }

  @Override
  public String createSiirtotiedostotForValintakoeOsallistumiset(
      LocalDateTime startDatetime, LocalDateTime endDatatime) {
    List<String> hakemusOids =
        tulosValintakoeOsallistuminenDAO.readNewOrModifiedHakemusOids(startDatetime, endDatatime);
    List<List<String>> partitions =
        Lists.partition(hakemusOids, siirtotiedostoS3Client.getMaxHakemusCountInFile());
    List<String> siirtotiedostoKeys = new ArrayList<>();
    for (List<String> hakemusOidChunk : partitions) {
      List<ValintakoeOsallistuminen> osallistumiset = new ArrayList<>();
      for (String hakemusOid : hakemusOidChunk) {
        osallistumiset.add(valintalaskentaTulosService.haeValintakoeOsallistumiset(hakemusOid));
      }
      siirtotiedostoKeys.add(
          siirtotiedostoS3Client.createSiirtotiedostoForValintakoeOsallistumiset(osallistumiset));
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
    List<String> hakukohdeOids =
        tulosValinnanvaiheDAO.readNewOrModifiedHakukohdeOids(startDatetime, endDatatime);
    List<List<String>> partitions =
        Lists.partition(hakukohdeOids, siirtotiedostoS3Client.getMaxHakukohdeCountInFile());
    List<String> siirtotiedostoKeys = new ArrayList<>();
    for (List<String> hakekohdeOidChunk : partitions) {
      List<List<ValintatietoValinnanvaiheDTO>> tulokset = new ArrayList<>();
      for (String hakukohdeOid : hakekohdeOidChunk) {

        tulokset.add(valintalaskentaTulosService.haeValinnanvaiheetHakukohteelle(hakukohdeOid));
      }
      siirtotiedostoKeys.add(
          siirtotiedostoS3Client.createSiirtotiedostoForValintalaskennanTulokset(
              tulokset.stream().flatMap(List::stream).collect(Collectors.toList())));
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
