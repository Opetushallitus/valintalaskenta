package fi.vm.sade.valintalaskenta.tulos.resource;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;

import fi.vm.sade.valintalaskenta.domain.dto.MuokattuJonosijaArvoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.MuokattuJonosijaDTO;
import fi.vm.sade.valintalaskenta.domain.dto.ValintatapajonoDTO;

@Path("valintatapajono")
@Api(value = "/valintatapajono", description = "Resurssi valintatapajonon jonosijojen muokkaamiseen manuaalisesti")
public interface ValintatapajonoResource {
    @POST
    @Consumes(MediaType.APPLICATION_JSON)
    @Produces(MediaType.APPLICATION_JSON)
    @Path("{valintatapajonoOid}/{hakemusOid}/{jarjestyskriteeriPrioriteetti}/jonosija")
    @ApiOperation(value = "Muokkaa jonosijaa", response = MuokattuJonosijaDTO.class)
    public Response muutaJonosija(
            @ApiParam(value = "Valintatapajonon OID", required = true) @PathParam("valintatapajonoOid") String valintatapajonoOid,
            @ApiParam(value = "Hakemus OID", required = true) @PathParam("hakemusOid") String hakemusOid,
            @ApiParam(value = "Muokattavan järjestyskriteerin prioriteetti", required = true) @PathParam("jarjestyskriteeriPrioriteetti") Integer jarjestyskriteeriPrioriteetti,
            @ApiParam(value = "Järjestyskriteerin uusi arvo", required = true) MuokattuJonosijaArvoDTO arvo);

    @DELETE
    @Consumes(MediaType.APPLICATION_JSON)
    @Produces(MediaType.APPLICATION_JSON)
    @Path("{valintatapajonoOid}/{hakemusOid}/{jarjestyskriteeriPrioriteetti}/jonosija")
    @ApiOperation(value = "Poista muokattu jonosijaa", response = MuokattuJonosijaDTO.class)
    public Response poistaMuokattuJonosija(
            @ApiParam(value = "Valintatapajonon OID", required = true) @PathParam("valintatapajonoOid") String valintatapajonoOid,
            @ApiParam(value = "Hakemus OID", required = true) @PathParam("hakemusOid") String hakemusOid,
            @ApiParam(value = "Muokattavan järjestyskriteerin prioriteetti", required = true) @PathParam("jarjestyskriteeriPrioriteetti") Integer jarjestyskriteeriPrioriteetti);

    @PUT
    @Consumes(MediaType.APPLICATION_JSON)
    @Produces(MediaType.APPLICATION_JSON)
    @Path("{valintatapajonoOid}/valmissijoiteltavaksi")
    @ApiOperation(value = "Lisää/Poistaa valintatapajonon sijoittelusta", response = ValintatapajonoDTO.class)
    public Response muokkaaSijotteluStatusta(
            @ApiParam(value = "Valintatapajonon OID", required = true) @PathParam("valintatapajonoOid") String valintatapajonoOid,
            @ApiParam(value = "Sijoittelustatus", required = true) @QueryParam("status") boolean status);

    @GET
    @Path("/{valintatapajonoOid}/valmissijoiteltavaksi")
    @Produces(MediaType.APPLICATION_JSON)
    public Response haeSijoitteluStatus(@ApiParam(value = "Valintatapajonon OID", required = true) @PathParam("valintatapajonoOid") String oid);
}
