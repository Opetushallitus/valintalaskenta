package fi.vm.sade.valintalaskenta.tulos.resource;

import fi.vm.sade.valintalaskenta.domain.dto.JarjestyskriteerihistoriaDTO;
import java.util.List;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

@Path("resources/jonosijahistoria")
public interface JonosijaHistoriaResource {
  @GET
  @Path("{valintatapajonoOid}/{hakemusOid}")
  @Produces(MediaType.APPLICATION_JSON)
  List<JarjestyskriteerihistoriaDTO> listJonosijaHistoria(
      @PathParam("valintatapajonoOid") final String valintatapajonoOid,
      @PathParam("hakemusOid") final String hakemusOid);
}
