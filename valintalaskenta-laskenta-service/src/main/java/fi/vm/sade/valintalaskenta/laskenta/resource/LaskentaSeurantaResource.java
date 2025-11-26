package fi.vm.sade.valintalaskenta.laskenta.resource;

import fi.vm.sade.valintalaskenta.domain.dto.seuranta.*;
import fi.vm.sade.valintalaskenta.laskenta.dao.SeurantaDao;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import java.time.Instant;
import java.util.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

@RestController
@PreAuthorize(
    "hasAnyRole('ROLE_APP_VALINTOJENTOTEUTTAMINEN_CRUD', 'ROLE_APP_VALINTOJENTOTEUTTAMINEN_READ_UPDATE')")
@RequestMapping(value = "/resources/seuranta")
@Tag(name = "/resources/seuranta", description = "Resurssi laskennan seurantaan")
public class LaskentaSeurantaResource {

  private static final Logger LOG = LoggerFactory.getLogger(LaskentaSeurantaResource.class);

  private final SeurantaDao seurantaDao;

  public LaskentaSeurantaResource(SeurantaDao seurantaDao) {
    this.seurantaDao = seurantaDao;
  }

  /** Yhteenvedot olemassa olevista laskennoista */
  @GetMapping(
      value = "/yhteenvetokaikillelaskennoille",
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Hakee yhteenvedot kaikista laskennoista")
  ResponseEntity<Collection<YhteenvetoDto>> haeYhteenvetoKaikilleLaskennoille() {
    return ResponseEntity.status(HttpStatus.OK)
        .body(
            seurantaDao.haeYhteenvetoKaikilleLaskennoille(
                Instant.now().minusSeconds(60 * 60 * 24)));
  }

  @GetMapping(value = "/laskenta/{uuid}", produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Palauttaa laskennan oid:n perusteella")
  ResponseEntity<LaskentaDto> laskenta(@PathVariable("uuid") String uuid) {
    try {
      Optional<LaskentaDto> laskenta = seurantaDao.haeLaskenta(uuid);
      if (!laskenta.isPresent()) {
        return ResponseEntity.status(HttpStatus.GONE).build();
      }
      return ResponseEntity.status(HttpStatus.OK).body(laskenta.get());
    } catch (Exception e) {
      throw e;
    }
  }

  @GetMapping(value = "/lataa/{uuid}", produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Palauttaa laskennan tiedostona uuid:n perusteella")
  ResponseEntity<LaskentaDto> lataa(@PathVariable("uuid") String uuid) {
    Optional<LaskentaDto> laskenta = seurantaDao.haeLaskenta(uuid);
    if (!laskenta.isPresent()) {
      return ResponseEntity.status(HttpStatus.GONE).build();
    }
    return ResponseEntity.status(HttpStatus.OK)
        .header(
            "Content-Disposition",
            "attachment; filename=laskenta_" + laskenta.get().getUuid() + ".json")
        .body(laskenta.get());
  }

  @GetMapping(value = "/yhteenveto/{uuid}", produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Palauttaa laskennan yhteenvedon uuid:n perusteella")
  ResponseEntity<YhteenvetoDto> yhteenveto(@PathVariable("uuid") String uuid) {
    Optional<YhteenvetoDto> yhteenveto = seurantaDao.haeYhteenveto(uuid);
    if (!yhteenveto.isPresent()) {
      return ResponseEntity.status(HttpStatus.GONE).build();
    }
    return ResponseEntity.status(HttpStatus.OK).body(yhteenveto.get());
  }
}
