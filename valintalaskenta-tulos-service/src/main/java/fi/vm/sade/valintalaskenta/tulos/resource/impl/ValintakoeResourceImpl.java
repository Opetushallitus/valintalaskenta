package fi.vm.sade.valintalaskenta.tulos.resource.impl;

import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.READ_UPDATE_CRUD;

import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.ValintakoeOsallistuminenDTO;
import fi.vm.sade.valintalaskenta.tulos.mapping.ValintalaskentaModelMapper;
import fi.vm.sade.valintalaskenta.tulos.resource.ValintakoeResource;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import java.util.List;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;

@Controller
@Path("valintakoe")
@PreAuthorize("isAuthenticated()")
@Api(value = "/valintakoe", description = "Resurssi valintakoeosallistumistulosten hakemiseen")
public class ValintakoeResourceImpl implements ValintakoeResource {
  private final ValintalaskentaTulosService tulosService;
  private final ValintalaskentaModelMapper modelMapper;

  @Autowired
  public ValintakoeResourceImpl(
      ValintalaskentaTulosService tulosService, ValintalaskentaModelMapper modelMapper) {
    this.tulosService = tulosService;
    this.modelMapper = modelMapper;
  }

  @GET
  @Produces(MediaType.APPLICATION_JSON)
  @Path("hakemus/{hakemusOid}")
  @PreAuthorize(READ_UPDATE_CRUD)
  @ApiOperation(
      value = "Hakee valintakoeosallistumiset hakemukselle OID:n perusteella",
      response = ValintakoeOsallistuminenDTO.class)
  public ValintakoeOsallistuminenDTO haku(
      @ApiParam(value = "Hakemus OID", required = true) @PathParam("hakemusOid")
          String hakemusOid) {
    return modelMapper.map(
        tulosService.haeValintakoeOsallistumiset(hakemusOid), ValintakoeOsallistuminenDTO.class);
  }

  @GET
  @Produces(MediaType.APPLICATION_JSON)
  @Path("hakutoive/{hakukohdeOid}")
  @PreAuthorize(READ_UPDATE_CRUD)
  @ApiOperation(
      value = "Hakee valintakoeosallistumiset hakukohteelle OID:n perusteella",
      response = ValintakoeOsallistuminenDTO.class)
  public List<ValintakoeOsallistuminenDTO> hakuByHakutoive(
      @ApiParam(value = "Hakukohde OID", required = true) @PathParam("hakukohdeOid")
          String hakukohdeOid) {
    return modelMapper.mapList(
        tulosService.haeValintakoeOsallistumisetByHakutoive(hakukohdeOid),
        ValintakoeOsallistuminenDTO.class);
  }
}
