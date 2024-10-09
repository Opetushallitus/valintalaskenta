package fi.vm.sade.valintalaskenta.tulos.service;

import com.google.gson.JsonObject;
import java.time.LocalDateTime;

public interface SiirtotiedostoService {
  JsonObject createSiirtotiedostotForValintakoeOsallistumiset(
      LocalDateTime startDatetime, LocalDateTime endDatatime);

  JsonObject createSiirtotiedostotForValintalaskennanTulokset(
      LocalDateTime startDatetime, LocalDateTime endDatatime);
}
