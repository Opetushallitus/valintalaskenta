package fi.vm.sade.valintalaskenta.tulos.resource.impl;

import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.READ_UPDATE_CRUD;

import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;
import fi.vm.sade.valintalaskenta.tulos.resource.HakemusResource;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;

@Controller
@Path("hakemus")
@PreAuthorize("isAuthenticated()")
@Api(value = "/hakemus", description = "Resurssi hakemuskohtaisten tulosten hakemiseen")
public class HakemusResourceImpl implements HakemusResource {
  @Autowired private ValintalaskentaTulosService tulosService;

  @GET
  @Path("{hakuoid}/{hakemusoid}")
  @Produces(MediaType.APPLICATION_JSON)
  @PreAuthorize(READ_UPDATE_CRUD)
  @ApiOperation(
      value = "Hakee hakemuksen tulokset haku OID:n ja hakemuksen OID:n perustella",
      response = HakemusDTO.class)
  public HakemusDTO hakemus(
      @ApiParam(value = "Haku OID", required = true) @PathParam("hakuoid") String hakuoid,
      @ApiParam(value = "Hakemus OID", required = true) @PathParam("hakemusoid")
          String hakemusoid) {
    return tulosService.haeTuloksetHakemukselle(hakuoid, hakemusoid);
  }
}
