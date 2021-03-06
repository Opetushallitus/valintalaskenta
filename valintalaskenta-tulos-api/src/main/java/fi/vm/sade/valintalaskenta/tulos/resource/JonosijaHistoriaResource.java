package fi.vm.sade.valintalaskenta.tulos.resource;

import fi.vm.sade.valintalaskenta.domain.dto.JarjestyskriteerihistoriaDTO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import java.util.List;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

@Path("jonosijahistoria")
@Api(value = "/jonosijahistoria", description = "Resurssi jonosijahistoriatiedon hakemiseen")
public interface JonosijaHistoriaResource {

  @GET
  @Path("{valintatapajonoOid}/{hakemusOid}")
  @Produces(MediaType.APPLICATION_JSON)
  @ApiOperation(
      value = "Hakee jonosijahistoriat valintatapajono OID:n ja hakemus OID:n perusteella",
      response = JarjestyskriteerihistoriaDTO.class)
  public List<JarjestyskriteerihistoriaDTO> listJonosijaHistoria(
      @ApiParam(value = "Valintatapajono OID", required = true) @PathParam("valintatapajonoOid")
          String valintatapajonoOid,
      @ApiParam(value = "Hakemus OID", required = true) @PathParam("hakemusOid") String hakemusOid);
}
