package fi.vm.sade.valintalaskenta.ovara.ajastus;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.gson.JsonObject;
import fi.vm.sade.valintalaskenta.ovara.ajastus.repository.SiirtotiedostoProsessiRepository;
import fi.vm.sade.valintalaskenta.tulos.service.impl.SiirtotiedostoServiceImpl;
import java.sql.Timestamp;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Service;

@Service
@Profile("ovara")
public class SiirtotiedostoAjastusService {

  ObjectMapper mapper = new ObjectMapper();

  private static final Logger logger = LoggerFactory.getLogger(SiirtotiedostoAjastusService.class);

  private final SiirtotiedostoProsessiRepository siirtotiedostoProsessiRepository;
  private final SiirtotiedostoServiceImpl siirtotiedostoService;

  public SiirtotiedostoAjastusService(
      SiirtotiedostoProsessiRepository siirtotiedostoProsessiRepository,
      SiirtotiedostoServiceImpl siirtotiedostoService) {
    this.siirtotiedostoProsessiRepository = siirtotiedostoProsessiRepository;
    this.siirtotiedostoService = siirtotiedostoService;
  }

  public void createNextSiirtotiedosto() {
    logger.info("Creating siirtotiedosto by ajastus!");
    SiirtotiedostoProsessi latest = siirtotiedostoProsessiRepository.findLatestSuccessful();
    logger.info("Latest: {}", latest);
    SiirtotiedostoProsessi uusi =
        siirtotiedostoProsessiRepository.save(latest.createNewProcessBasedOnThis());
    logger.info("New process: {}", uusi);

    try {

      JsonObject osallistumisetResult =
          siirtotiedostoService.createSiirtotiedostotForValintakoeOsallistumiset(
              uusi.getWindowStart().toLocalDateTime(), uusi.getWindowEnd().toLocalDateTime());
      JsonObject tuloksetResult =
          siirtotiedostoService.createSiirtotiedostotForValintalaskennanTulokset(
              uusi.getWindowStart().toLocalDateTime(), uusi.getWindowEnd().toLocalDateTime());
      Boolean bothSuccess =
          osallistumisetResult.get("success").getAsBoolean()
              && tuloksetResult.get("success").getAsBoolean();

      logger.info("Osallistumiset: {}", osallistumisetResult);
      logger.info("Tulokset: {}", tuloksetResult);

      if (bothSuccess) {
        uusi.setSuccess(true);
      } else {
        throw new RuntimeException("Kaikkien siirtotiedostojen muodostaminen ei onnistunut!");
      }
      uusi.setInfo(
          new SiirtotiedostoInfo(
              Integer.parseInt(tuloksetResult.get("total").toString()),
              Integer.parseInt(osallistumisetResult.get("total").toString())));
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
      siirtotiedostoProsessiRepository.save(uusi);
    }
  }
}
