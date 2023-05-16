package fi.vm.sade.valintalaskenta.tulos.resource.impl;

import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.READ_UPDATE_CRUD;

import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;
import fi.vm.sade.valintalaskenta.tulos.resource.HakemusResource;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;

@Controller
@PreAuthorize("isAuthenticated()")
@Api(value = "/hakemus", description = "Resurssi hakemuskohtaisten tulosten hakemiseen")
@RequestMapping(value = "/hakemus")
public class HakemusResourceImpl implements HakemusResource {
  private final ValintalaskentaTulosService tulosService;

  @Autowired
  public HakemusResourceImpl(ValintalaskentaTulosService tulosService) {
    this.tulosService = tulosService;
  }

  @PreAuthorize(READ_UPDATE_CRUD)
  @ApiOperation(
      value = "Hakee hakemuksen tulokset haku OID:n ja hakemuksen OID:n perustella",
      response = HakemusDTO.class)
  @GetMapping(value = "/{hakuoid}/{hakemusoid}", produces = MediaType.APPLICATION_JSON_VALUE)
  public HakemusDTO hakemus(
      @ApiParam(value = "Haku OID", required = true) @PathVariable("hakuoid") final String hakuoid,
      @ApiParam(value = "Hakemus OID", required = true) @PathVariable("hakemusoid")
          final String hakemusoid) {
    return tulosService.haeTuloksetHakemukselle(hakuoid, hakemusoid);
  }
}
