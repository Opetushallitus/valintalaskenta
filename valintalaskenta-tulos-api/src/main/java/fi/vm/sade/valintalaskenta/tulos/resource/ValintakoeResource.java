package fi.vm.sade.valintalaskenta.tulos.resource;

import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.ValintakoeOsallistuminenDTO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import java.util.List;
import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

@Path("valintakoe")
@Api(value = "/valintakoe", description = "Resurssi valintakoeosallistumistulosten hakemiseen")
public interface ValintakoeResource {
  @GET
  @Produces(MediaType.APPLICATION_JSON)
  @Path("hakemus/{hakemusOid}")
  @ApiOperation(
      value = "Hakee valintakoeosallistumiset hakemukselle OID:n perusteella",
      response = ValintakoeOsallistuminenDTO.class)
  ValintakoeOsallistuminenDTO haku(
      @ApiParam(value = "Hakemus OID", required = true) @PathParam("hakemusOid") String hakemusOid);

  @GET
  @Produces(MediaType.APPLICATION_JSON)
  @Path("hakutoive/{hakukohdeOid}")
  @ApiOperation(
      value = "Hakee valintakoeosallistumiset hakukohteelle OID:n perusteella",
      response = ValintakoeOsallistuminenDTO.class)
  List<ValintakoeOsallistuminenDTO> hakuByHakutoive(
      @ApiParam(value = "Hakukohde OID", required = true) @PathParam("hakukohdeOid")
          String hakukohdeOid);

  @POST
  @Consumes(MediaType.APPLICATION_JSON)
  @Produces(MediaType.APPLICATION_JSON)
  @Path("hakutoive")
  @ApiOperation(
          value = "Hakee valintakoeosallistumiset hakukohteille OID:n perusteella",
          response = ValintakoeOsallistuminenDTO.class)
  List<ValintakoeOsallistuminenDTO> hakuByOids(
          @ApiParam(value = "Hakukohde OIDS", required = true)
                  List<String> hakukohdeOids);
}
