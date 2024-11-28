package fi.vm.sade.valintalaskenta.tulos.resource.impl;

import static fi.vm.sade.valintalaskenta.domain.dto.siirtotiedosto.SiirtotiedostoConstants.*;
import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.READ_UPDATE_CRUD;
import static org.apache.commons.lang3.StringUtils.isBlank;

import fi.vm.sade.valintalaskenta.tulos.service.SiirtotiedostoService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import java.time.LocalDateTime;
import java.time.ZonedDateTime;
import java.time.format.DateTimeParseException;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@PreAuthorize("isAuthenticated()")
@Tag(
    name = "/resources/siirtotiedosto",
    description =
        "Resurssi raportoinnin siirtotiedstojen luontiin valintalaskennan tulosten pohjalta")
@RequestMapping(value = "/resources/siirtotiedosto")
public class SiirtotiedostoResourceImpl {
  private final SiirtotiedostoService siirtotiedostoService;

  public SiirtotiedostoResourceImpl(final SiirtotiedostoService siirtotiedostoService) {
    this.siirtotiedostoService = siirtotiedostoService;
  }

  private LocalDateTime parseDateTime(
      String dateTimeStr, String fieldName, ZonedDateTime defaultDateTime) {
    try {
      if (!isBlank(dateTimeStr)) {
        return ZonedDateTime.parse(dateTimeStr, SIIRTOTIEDOSTO_DATETIME_FORMATTER)
            .toLocalDateTime();
      }
      return defaultDateTime != null ? defaultDateTime.toLocalDateTime() : null;
    } catch (DateTimeParseException dtpe) {
      throw new IllegalArgumentException(
          String.format(
              "Virheellinen arvo kentälle %s, vaadittu formaati: '%s'",
              fieldName, SIIRTOTIEDOSTO_DATETIME_FORMAT));
    }
  }

  @PreAuthorize(READ_UPDATE_CRUD)
  @GetMapping(value = "/valintakoeosallistumiset", produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(
      summary =
          "Luo siirtotiedostot annetulla aikavälillä luoduista / muutetuista valintakoeosallistumisista hakemuksittain")
  public ResponseEntity<String> valintakoeOsallistumiset(
      @Parameter(description = "Alkuaika") @RequestParam(required = false) String startDatetime,
      @Parameter(description = "Loppuaika") @RequestParam(required = false) String endDatetime) {
    LocalDateTime start = parseDateTime(startDatetime, "Alkuaika", null);
    LocalDateTime end =
        parseDateTime(endDatetime, "Loppuaika", ZonedDateTime.now(SIIRTOTIEDOSTO_TIMEZONE));
    String response =
        siirtotiedostoService
            .createSiirtotiedostotForValintakoeOsallistumiset(start, end)
            .toString();
    return new ResponseEntity<>(response, HttpStatus.OK);
  }

  @PreAuthorize(READ_UPDATE_CRUD)
  @GetMapping(value = "/valintalaskennantulokset", produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(
      summary =
          "Luo siirtotiedostot annetulla aikavälillä luoduista / muutetuista valintalaskennan tuloksista hakukohteittain")
  public ResponseEntity<String> valintalaskennanTulokset(
      @Parameter(description = "Alkuaika") @RequestParam(required = false) String startDatetime,
      @Parameter(description = "Loppuaika") @RequestParam(required = false) String endDatetime) {
    LocalDateTime start = parseDateTime(startDatetime, "Alkuaika", null);
    LocalDateTime end =
        parseDateTime(endDatetime, "Loppuaika", ZonedDateTime.now(SIIRTOTIEDOSTO_TIMEZONE));
    String response =
        siirtotiedostoService
            .createSiirtotiedostotForValintalaskennanTulokset(start, end)
            .toString();
    return new ResponseEntity<>(response, HttpStatus.OK);
  }
}
