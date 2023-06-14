package fi.vm.sade.valintalaskenta.tulos.resource;

import fi.vm.sade.service.valintaperusteet.dto.ValintatapajonoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.JonoDto;
import fi.vm.sade.valintalaskenta.domain.dto.MuokattuJonosijaArvoDTO;
import java.util.List;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.*;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

@Path("resources/valintatapajono")
public interface ValintatapajonoResource {
  @POST
  @Consumes(MediaType.APPLICATION_JSON)
  @Produces(MediaType.APPLICATION_JSON)
  @Path("{valintatapajonoOid}/{hakemusOid}/{jarjestyskriteeriPrioriteetti}/jonosija")
  Response muutaJonosija(
      @PathParam("valintatapajonoOid") final String valintatapajonoOid,
      @PathParam("hakemusOid") final String hakemusOid,
      @PathParam("jarjestyskriteeriPrioriteetti") final Integer jarjestyskriteeriPrioriteetti,
      final MuokattuJonosijaArvoDTO arvo,
      @Context final HttpServletRequest request);

  @DELETE
  @Consumes(MediaType.APPLICATION_JSON)
  @Produces(MediaType.APPLICATION_JSON)
  @Path("{valintatapajonoOid}/{hakemusOid}/{jarjestyskriteeriPrioriteetti}/jonosija")
  Response poistaMuokattuJonosija(
      @PathParam("valintatapajonoOid") final String valintatapajonoOid,
      @PathParam("hakemusOid") final String hakemusOid,
      @PathParam("jarjestyskriteeriPrioriteetti") final Integer jarjestyskriteeriPrioriteetti,
      @Context final HttpServletRequest request);

  @PUT
  @Consumes(MediaType.APPLICATION_JSON)
  @Produces(MediaType.APPLICATION_JSON)
  @Path("{valintatapajonoOid}/valmissijoiteltavaksi")
  Response muokkaaSijotteluStatusta(
      @PathParam("valintatapajonoOid") final String valintatapajonoOid,
      @QueryParam("status") final boolean status,
      final ValintatapajonoDTO valintapajono,
      @Context final HttpServletRequest request);

  @GET
  @Path("/{valintatapajonoOid}/valmissijoiteltavaksi")
  @Produces(MediaType.APPLICATION_JSON)
  Response haeSijoitteluStatus(@PathParam("valintatapajonoOid") final String oid);

  @GET
  @Path("/jonotsijoittelussa/{hakuOid}")
  @Produces("application/json")
  List<JonoDto> jonotSijoittelussa(@PathParam("hakuOid") final String hakuOid);
}
