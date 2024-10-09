package fi.vm.sade.valintalaskenta.ovara.ajastus;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import fi.vm.sade.valintalaskenta.ovara.ajastus.impl.SiirtotiedostoProsessiRepositoryImpl;
import fi.vm.sade.valintalaskenta.tulos.service.impl.SiirtotiedostoServiceImpl;
import java.sql.Timestamp;
import java.util.HashMap;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Service;

@Service
@Profile("ovara")
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
      Map<String, String> infoMap = new HashMap<>();

      String osallistumisetResult = siirtotiedostoService.createSiirtotiedostotForValintakoeOsallistumiset(uusi.getWindowStart().toLocalDateTime(), uusi.getWindowEnd().toLocalDateTime());
      String tuloksetResult = siirtotiedostoService.createSiirtotiedostotForValintalaskennanTulokset(uusi.getWindowStart().toLocalDateTime(), uusi.getWindowEnd().toLocalDateTime());
      logger.info("Osallistumiset: {}", osallistumisetResult);
      logger.info("Tulokset: {}", tuloksetResult);
      infoMap.put("Tulokset", tuloksetResult);
      infoMap.put("Osallistumiset", osallistumisetResult);

      //Todo, infon sisältö on nyt vähän ruma.
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
      uusi.setRunEnd(new Timestamp(System.currentTimeMillis()));
      siirtotiedostoProsessiRepository.persist(uusi);
    }
    return "DONE";
  }
}
