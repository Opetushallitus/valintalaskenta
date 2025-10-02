package fi.vm.sade.valintalaskenta.laskenta.resource;

import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.OPH_CRUD;

import fi.vm.sade.valintalaskenta.domain.dto.valintapiste.PistetietoWrapper;
import fi.vm.sade.valintalaskenta.laskenta.service.valintapiste.ValintapisteService;
import io.swagger.v3.oas.annotations.Operation;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@PreAuthorize("isAuthenticated()")
@RequestMapping(value = "/resources/valintapisteet")
public class ValintapisteResource {

  private final ValintapisteService valintapisteService;

  @Autowired
  public ValintapisteResource(ValintapisteService valintapisteService) {
    this.valintapisteService = valintapisteService;
  }

  @PreAuthorize(OPH_CRUD)
  @GetMapping(
      value = "/hakemus/{hakemusOid}/oppijaOid/{oppijaOid}",
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Hakemuksen pistetiedot")
  public ResponseEntity<PistetietoWrapper> findValintapisteetForHakemus(
      @PathVariable String hakemusOid, @PathVariable String oppijaOid) {
    return withLastModified(
        valintapisteService.findValintapisteetForHakemus(hakemusOid, oppijaOid));
  }

  @PreAuthorize(OPH_CRUD)
  @PostMapping(
      value = "/pisteet-with-hakemusoids",
      produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Returns pistetiedot for hakemus (max 32767)")
  public ResponseEntity<List<PistetietoWrapper>> findValintapisteetWithHakemusoids(
      @RequestBody List<String> hakemusOids) {
    return withLastModified(valintapisteService.findValintapisteetForHakemukset(hakemusOids));
  }

  private <T> ResponseEntity<T> withLastModified(Pair<ZonedDateTime, T> pair) {
    return withLastModified(pair.getLeft(), pair.getRight());
  }

  private <T> ResponseEntity<T> withLastModified(ZonedDateTime lastModified, T body) {

    ResponseEntity.BodyBuilder builder = ResponseEntity.status(HttpStatus.OK);
    if (lastModified != null) {
      builder.header("Last-Modified", lastModified.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME));
    }
    return builder.body(body);
  }
}
