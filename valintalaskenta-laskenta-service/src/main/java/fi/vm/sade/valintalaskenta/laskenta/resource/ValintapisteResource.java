package fi.vm.sade.valintalaskenta.laskenta.resource;

import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.OPH_CRUD;

import fi.vm.sade.valintalaskenta.domain.dto.valintapiste.ValintapisteDTO;
import fi.vm.sade.valintalaskenta.domain.valintapiste.Valintapiste;
import fi.vm.sade.valintalaskenta.laskenta.service.valintapiste.ValintapisteService;
import io.swagger.v3.oas.annotations.Operation;
import java.util.Collection;
import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.security.access.prepost.PreAuthorize;
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
  @PostMapping(
      value = "/pisteet-with-hakemusoids",
      produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Returns pistetiedot for hakemus (max 32767)")
  public Collection<ValintapisteDTO> findValintapisteetWithHakemusoids(
      @RequestBody List<String> hakemusOids) {
    return valintapisteService.findValintapisteetForHakemukset(hakemusOids).stream()
        .map(Valintapiste::toDTO)
        .toList();
  }
}
