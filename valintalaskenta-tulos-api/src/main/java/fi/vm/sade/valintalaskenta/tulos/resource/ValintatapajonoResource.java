package fi.vm.sade.valintalaskenta.tulos.resource;

import fi.vm.sade.service.valintaperusteet.dto.ValintatapajonoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.JonoDto;
import fi.vm.sade.valintalaskenta.domain.dto.MuokattuJonosijaArvoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.MuokattuJonosijaDTO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import java.util.List;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.*;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

@Path("valintatapajono")
@Api(
    value = "/valintatapajono",
    description = "Resurssi valintatapajonon jonosijojen muokkaamiseen manuaalisesti")
public interface ValintatapajonoResource {
  @POST
  @Consumes(MediaType.APPLICATION_JSON)
  @Produces(MediaType.APPLICATION_JSON)
  @Path("{valintatapajonoOid}/{hakemusOid}/{jarjestyskriteeriPrioriteetti}/jonosija")
  @ApiOperation(value = "Muokkaa jonosijaa", response = MuokattuJonosijaDTO.class)
  public Response muutaJonosija(
      @ApiParam(value = "Valintatapajonon OID", required = true) @PathParam("valintatapajonoOid")
          String valintatapajonoOid,
      @ApiParam(value = "Hakemus OID", required = true) @PathParam("hakemusOid") String hakemusOid,
      @ApiParam(value = "Muokattavan järjestyskriteerin prioriteetti", required = true)
          @PathParam("jarjestyskriteeriPrioriteetti")
          Integer jarjestyskriteeriPrioriteetti,
      @ApiParam(value = "Järjestyskriteerin uusi arvo", required = true)
          MuokattuJonosijaArvoDTO arvo,
      @Context HttpServletRequest request);

  @DELETE
  @Consumes(MediaType.APPLICATION_JSON)
  @Produces(MediaType.APPLICATION_JSON)
  @Path("{valintatapajonoOid}/{hakemusOid}/{jarjestyskriteeriPrioriteetti}/jonosija")
  @ApiOperation(value = "Poista muokattu jonosijaa", response = MuokattuJonosijaDTO.class)
  public Response poistaMuokattuJonosija(
      @ApiParam(value = "Valintatapajonon OID", required = true) @PathParam("valintatapajonoOid")
          String valintatapajonoOid,
      @ApiParam(value = "Hakemus OID", required = true) @PathParam("hakemusOid") String hakemusOid,
      @ApiParam(value = "Muokattavan järjestyskriteerin prioriteetti", required = true)
          @PathParam("jarjestyskriteeriPrioriteetti")
          Integer jarjestyskriteeriPrioriteetti,
      @Context HttpServletRequest request);

  @PUT
  @Consumes(MediaType.APPLICATION_JSON)
  @Produces(MediaType.APPLICATION_JSON)
  @Path("{valintatapajonoOid}/valmissijoiteltavaksi")
  @ApiOperation(value = "Tallentaa/muokkaa valintatapajonoa", response = ValintatapajonoDTO.class)
  public Response muokkaaSijotteluStatusta(
      @ApiParam(value = "Valintatapajonon OID", required = true) @PathParam("valintatapajonoOid")
          String valintatapajonoOid,
      @ApiParam(value = "Sijoittelustatus", required = true) @QueryParam("status") boolean status,
      @ApiParam(value = "Valintatapajono", required = true) ValintatapajonoDTO valintapajono,
      @Context HttpServletRequest request);

  @GET
  @Path("/{valintatapajonoOid}/valmissijoiteltavaksi")
  @Produces(MediaType.APPLICATION_JSON)
  public Response haeSijoitteluStatus(
      @ApiParam(value = "Valintatapajonon OID", required = true) @PathParam("valintatapajonoOid")
          String oid);

  @GET
  @Path("/jonotsijoittelussa/{hakuOid}")
  @Produces("application/json")
  public List<JonoDto> jonotSijoittelussa(@PathParam("hakuOid") String hakuOid);
}
