package fi.vm.sade.valintalaskenta.laskenta.resource.external;

import fi.vm.sade.sijoittelu.tulos.dto.HakukohdeDTO;
import fi.vm.sade.sijoittelu.tulos.dto.ValisijoitteluDTO;
import java.util.List;
import javax.ws.rs.*;

@Path("/sijoittelu-service/resources/valisijoittele")
public interface ValiSijoitteluResource {

  @POST
  @Path("{hakuOid}")
  @Consumes("application/json")
  @Produces("application/json")
  public List<HakukohdeDTO> sijoittele(
      @PathParam("hakuOid") String hakuOid, ValisijoitteluDTO hakukohteet);
}
