package fi.vm.sade.valintalaskenta.tulos.resource;

import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

@Path("resources/hakemus")
public interface HakemusResource {
  @GET
  @Path("{hakuoid}/{hakemusoid}")
  @Produces(MediaType.APPLICATION_JSON)
  HakemusDTO hakemus(
      @PathParam("hakuoid") final String hakuoid, @PathParam("hakemusoid") final String hakemusoid);
}
