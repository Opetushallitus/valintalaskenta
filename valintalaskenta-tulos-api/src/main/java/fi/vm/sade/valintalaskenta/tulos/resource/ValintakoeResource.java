package fi.vm.sade.valintalaskenta.tulos.resource;

import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.ValintakoeOsallistuminenDTO;
import java.util.List;
import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;

@Path("resources/valintakoe")
public interface ValintakoeResource {
  @GET
  @Produces(MediaType.APPLICATION_JSON)
  @Path("hakemus/{hakemusOid}")
  ValintakoeOsallistuminenDTO haku(@PathParam("hakemusOid") final String hakemusOid);

  @GET
  @Produces(MediaType.APPLICATION_JSON)
  @Path("hakutoive/{hakukohdeOid}")
  List<ValintakoeOsallistuminenDTO> hakuByHakutoive(
      @PathParam("hakukohdeOid") final String hakukohdeOid);

  @POST
  @Consumes(MediaType.APPLICATION_JSON)
  @Produces(MediaType.APPLICATION_JSON)
  @Path("hakutoive")
  List<ValintakoeOsallistuminenDTO> hakuByOids(final List<String> hakukohdeOids);

  @POST
  @Consumes(MediaType.APPLICATION_JSON)
  @Produces(MediaType.APPLICATION_JSON)
  @Path("hakijat")
  List<ValintakoeOsallistuminenDTO> hakijatByOids(final List<String> hakijaOids);
}
