package fi.vm.sade.valintalaskenta.laskenta.resource.external;

import fi.vm.sade.service.valintaperusteet.dto.*;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.List;
import java.util.Map;

@Path("/valintaperusteet-service/resources/valintalaskentakoostepalvelu/valintatapajono")
public interface ValintaperusteetValintatapajonoResource {

    @GET
    @Produces(MediaType.APPLICATION_JSON)
    @Path("/kopiot")
    Map<String, List<String>> findKopiot(@QueryParam("oid") List<String> oid);
}
