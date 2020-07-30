package fi.vm.sade.valintalaskenta.tulos.resource;

import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.HakemusOsallistuminenDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.HakuDTO;
import java.util.List;
import javax.ws.rs.*;

@Path("valintatieto")
public interface ValintatietoResource {
  @POST
  @Path("hakukohde/{hakukohdeOid}")
  @Consumes("application/json")
  @Produces("application/json")
  List<HakemusOsallistuminenDTO> haeValintatiedotHakukohteelle(
      @PathParam("hakukohdeOid") String hakukohdeOid, List<String> valintakoeTunnisteet);

  @GET
  @Path("haku/{hakuOid}")
  @Produces("application/json")
  HakuDTO haeValintatiedot(@PathParam("hakuOid") String hakuOid);
}
