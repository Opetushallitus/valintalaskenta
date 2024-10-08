package fi.vm.sade.valintalaskenta.ovara.ajastus;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import fi.vm.sade.valintalaskenta.ovara.ajastus.impl.SiirtotiedostoProsessiRepositoryImpl;
import fi.vm.sade.valintalaskenta.tulos.service.impl.SiirtotiedostoServiceImpl;
import java.time.OffsetDateTime;
import java.util.HashMap;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

@Service
public class SiirtotiedostoAjastusService {

  ObjectMapper mapper = new ObjectMapper();

  private static final Logger logger = LoggerFactory.getLogger(SiirtotiedostoAjastusService.class);

  private final SiirtotiedostoProsessiRepositoryImpl siirtotiedostoProsessiRepository;
  private final SiirtotiedostoServiceImpl siirtotiedostoService;

  public SiirtotiedostoAjastusService(
      SiirtotiedostoProsessiRepositoryImpl siirtotiedostoProsessiRepository,
      SiirtotiedostoServiceImpl siirtotiedostoService) {
    this.siirtotiedostoProsessiRepository = siirtotiedostoProsessiRepository;
    this.siirtotiedostoService = siirtotiedostoService;
  }

  public String createNextSiirtotiedosto() {
    logger.info("Creating siirtotiedosto by ajastus!");
    SiirtotiedostoProsessi latest = siirtotiedostoProsessiRepository.findLatestSuccessful();
    logger.info("Latest: {}", latest);
    SiirtotiedostoProsessi uusi = latest.createNewProcessBasedOnThis();
    logger.info("New process: {}", uusi);
    siirtotiedostoProsessiRepository.persist(uusi);

    try {
      Map<String, Integer> infoMap = new HashMap<>();
      int total = 0;
      logger.info("Todo, actually do it :)");
      // String resultInfo =
      //        siirtotiedostoServiceImpl.createSiirtotiedostot(
      //                uusi.getWindowStart().toLocalDateTime(),
      // uusi.getWindowEnd().toLocalDateTime());
      infoMap.put("tulokset", total);
      JsonNode jsonNode = mapper.valueToTree(infoMap);
      uusi.setSuccess(true);
      uusi.setInfo(jsonNode.toString());
    } catch (Exception e) {
      logger.error(
          "{} Tapahtui virhe muodostettaessa ajastettua siirtotiedostoa:",
          uusi.getExecutionUuid(),
          e);
      uusi.setSuccess(false);
      uusi.setErrorMessage(e.getMessage());
      uusi.setInfo(null);
    } finally {
      uusi.setRunEnd(OffsetDateTime.now());
      siirtotiedostoProsessiRepository.persist(uusi);
    }
    return "DONE";
  }
}
