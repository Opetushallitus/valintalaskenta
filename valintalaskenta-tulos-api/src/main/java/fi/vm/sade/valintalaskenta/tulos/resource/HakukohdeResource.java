package fi.vm.sade.valintalaskenta.tulos.resource;

import fi.vm.sade.valintalaskenta.domain.dto.HakijaryhmaDTO;
import fi.vm.sade.valintalaskenta.domain.dto.ValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.ValintatietoValinnanvaiheDTO;
import java.util.List;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.*;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import org.springframework.http.ResponseEntity;

@Path("resources/hakukohde")
public interface HakukohdeResource {
  @GET
  @Path("{hakukohdeoid}/valinnanvaihe")
  @Produces(MediaType.APPLICATION_JSON)
  List<ValintatietoValinnanvaiheDTO> hakukohde(
      @PathParam("hakukohdeoid") final String hakukohdeoid);

  @POST
  @Path("{hakukohdeoid}/valinnanvaihe")
  @Produces(MediaType.APPLICATION_JSON)
  @Consumes(MediaType.APPLICATION_JSON)
  ResponseEntity<Object> lisaaTuloksia(
      @PathParam("hakukohdeoid") final String hakukohdeoid,
      @QueryParam("tarjoajaOid") final String tarjoajaOid,
      final ValinnanvaiheDTO vaihe,
      @Context final HttpServletRequest request);

  @GET
  @Path("{hakukohdeoid}/hakijaryhma")
  @Produces(MediaType.APPLICATION_JSON)
  List<HakijaryhmaDTO> hakijaryhmat(@PathParam("hakukohdeoid") final String hakukohdeoid);
}
