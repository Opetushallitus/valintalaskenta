package fi.vm.sade.valintalaskenta.ovara.ajastus;

import com.fasterxml.jackson.databind.ObjectMapper;
import fi.vm.sade.valintalaskenta.domain.dto.siirtotiedosto.SiirtotiedostoResult;
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

      SiirtotiedostoResult osallistumisetResult =
          siirtotiedostoService.createSiirtotiedostotForValintakoeOsallistumiset(
              uusi.getWindowStart().toLocalDateTime(), uusi.getWindowEnd().toLocalDateTime());
      SiirtotiedostoResult tuloksetResult =
          siirtotiedostoService.createSiirtotiedostotForValintalaskennanTulokset(
              uusi.getWindowStart().toLocalDateTime(), uusi.getWindowEnd().toLocalDateTime());
      SiirtotiedostoResult pisteetResult =
          siirtotiedostoService.createSiirtotiedostotForValintapisteet(
              uusi.getWindowStart().toLocalDateTime(), uusi.getWindowEnd().toLocalDateTime());
      boolean allSuccess =
          osallistumisetResult.success() && tuloksetResult.success() && pisteetResult.success();

      logger.info("Osallistumiset: {}", osallistumisetResult);
      logger.info("Tulokset: {}", tuloksetResult);
      logger.info("Valintapisteet: {}", pisteetResult);

      if (allSuccess) {
        uusi.setSuccess(true);
      } else {
        throw new RuntimeException("Kaikkien siirtotiedostojen muodostaminen ei onnistunut!");
      }
      uusi.setInfo(
          new SiirtotiedostoInfo(
              tuloksetResult.total(), osallistumisetResult.total(), pisteetResult.total()));
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
      logger.info("Persistoidaan prosessi {}", uusi);
      siirtotiedostoProsessiRepository.save(uusi);
    }
  }
}
