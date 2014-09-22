package fi.vm.sade.valintalaskenta.laskenta.resource.external;

import fi.vm.sade.sijoittelu.tulos.dto.HakukohdeDTO;
import fi.vm.sade.sijoittelu.tulos.dto.ValisijoitteluDTO;
import org.springframework.stereotype.Controller;

import javax.ws.rs.*;
import java.util.List;
import java.util.Map;

@Controller
@Path("valisijoittele")
public interface SijoitteluResource {

    @POST
    @Path("{hakuOid}")
    @Consumes("application/json")
    @Produces("application/json")
    public List<HakukohdeDTO> sijoittele(@PathParam("hakuOid") String hakuOid, ValisijoitteluDTO hakukohteet);

}
