package fi.vm.sade.valintalaskenta.tulos.service;

import fi.vm.sade.valintalaskenta.domain.dto.siirtotiedosto.SiirtotiedostoResult;
import java.time.LocalDateTime;

public interface SiirtotiedostoService {
  SiirtotiedostoResult createSiirtotiedostotForValintakoeOsallistumiset(
      LocalDateTime startDatetime, LocalDateTime endDatatime);

  SiirtotiedostoResult createSiirtotiedostotForValintalaskennanTulokset(
      LocalDateTime startDatetime, LocalDateTime endDatatime);

  SiirtotiedostoResult createSiirtotiedostotForValintapisteet(
      LocalDateTime startDatetime, LocalDateTime endDatatime);
}
