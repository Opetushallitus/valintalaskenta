package fi.vm.sade.valintalaskenta.tulos.resource;

import fi.vm.sade.valintalaskenta.domain.dto.HakukohdeDTO;
import fi.vm.sade.valintalaskenta.domain.dto.MinimalJonoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.ValintakoeOsallistuminenDTO;
import java.util.List;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

@Path("resources/haku")
public interface HakuResource {
  @GET
  @Path("{hakuOid}/virheet")
  @Produces(MediaType.APPLICATION_JSON)
  List<HakukohdeDTO> virheet(@PathParam("hakuOid") final String hakuOid);

  @GET
  @Path("{hakuOid}/valintakoevirheet")
  @Produces(MediaType.APPLICATION_JSON)
  List<ValintakoeOsallistuminenDTO> valintakoevirheet(@PathParam("hakuOid") final String hakuOid);

  @GET
  @Path("/ilmanvalintalaskentaasijoitteluun")
  @Produces(MediaType.APPLICATION_JSON)
  List<MinimalJonoDTO> jonotSijoitteluun();
}
