package fi.vm.sade.valintalaskenta.domain.resource;

import fi.vm.sade.valintalaskenta.domain.dto.LaskeDTO;

import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;

/**
 * Created by jukais on 24.3.2014.
 */
@Path("valintalaskenta")
public interface ValintalaskentaResource {
    //	@PreAuthorize(CRUD)
    @POST
    @Path("laske")
    @Consumes("application/json")
    @Produces("text/plain")
    String laske(LaskeDTO laskeDTO);

    //	@PreAuthorize(CRUD)
    @POST
    @Path("valintakokeet")
    @Consumes("application/json")
    @Produces("text/plain")
    String valintakokeet(LaskeDTO laskeDTO);

    @Path("laskekaikki")
    @Consumes("application/json")
    @Produces("text/plain")
    @POST
    public String laskeKaikki(LaskeDTO laskeDTO);
}
