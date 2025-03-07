package fi.vm.sade.valintalaskenta.tulos.service;

import java.time.LocalDateTime;

public interface SiirtotiedostoService {
  String createSiirtotiedostotForValintakoeOsallistumiset(
      LocalDateTime startDatetime, LocalDateTime endDatatime);

  String createSiirtotiedostotForValintalaskennanTulokset(
      LocalDateTime startDatetime, LocalDateTime endDatatime);
}
