package fi.vm.sade.valintalaskenta.laskenta.resource.external;

import fi.vm.sade.sijoittelu.tulos.dto.ValisijoitteluDTO;
import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;

@Path("erillissijoittele")
public interface ErillisSijoitteluResource {

  @POST
  @Path("{hakuOid}")
  @Consumes("application/json")
  @Produces("application/json")
  Long sijoittele(@PathParam("hakuOid") String hakuOid, ValisijoitteluDTO hakukohteet);
}
