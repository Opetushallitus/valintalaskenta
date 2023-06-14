package fi.vm.sade.valintalaskenta.tulos.resource.impl;

import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.READ_UPDATE_CRUD;

import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;
import fi.vm.sade.valintalaskenta.tulos.resource.HakemusResource;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@PreAuthorize("isAuthenticated()")
@Tag(name = "/resources/hakemus", description = "Resurssi hakemuskohtaisten tulosten hakemiseen")
@RequestMapping(value = "/resources/hakemus")
public class HakemusResourceImpl implements HakemusResource {
  private final ValintalaskentaTulosService tulosService;

  @Autowired
  public HakemusResourceImpl(ValintalaskentaTulosService tulosService) {
    this.tulosService = tulosService;
  }

  @PreAuthorize(READ_UPDATE_CRUD)
  @Operation(summary = "Hakee hakemuksen tulokset haku OID:n ja hakemuksen OID:n perustella")
  @GetMapping(value = "/{hakuoid}/{hakemusoid}", produces = MediaType.APPLICATION_JSON_VALUE)
  public HakemusDTO hakemus(
      @Parameter(name = "hakuoid", required = true) @PathVariable("hakuoid") final String hakuoid,
      @Parameter(name = "hakemusoid", required = true) @PathVariable("hakemusoid")
          final String hakemusoid) {
    return tulosService.haeTuloksetHakemukselle(hakuoid, hakemusoid);
  }
}
