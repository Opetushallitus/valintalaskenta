package fi.vm.sade.valintalaskenta.laskenta.resource.external;

import fi.vm.sade.sijoittelu.tulos.dto.ValisijoitteluDTO;
import org.springframework.stereotype.Controller;

import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;

@Controller
@Path("erillissijoittele")
public interface ErillisSijoitteluResource {

    @POST
    @Path("{hakuOid}")
    @Consumes("application/json")
    @Produces("application/json")
    long sijoittele(@PathParam("hakuOid") String hakuOid, ValisijoitteluDTO hakukohteet);

}
