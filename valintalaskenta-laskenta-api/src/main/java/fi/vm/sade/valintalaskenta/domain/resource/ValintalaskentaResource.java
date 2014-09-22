package fi.vm.sade.valintalaskenta.domain.resource;

import fi.vm.sade.valintalaskenta.domain.dto.LaskeDTO;

import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import java.util.List;

/**
 * Created by jukais on 24.3.2014.
 */
@Path("valintalaskenta")
public interface ValintalaskentaResource {

    @POST
    @Path("laske")
    @Consumes("application/json")
    @Produces("text/plain")
    String laske(LaskeDTO laskeDTO);


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

    @Path("laskejasijoittele")
    @Consumes("application/json")
    @Produces("text/plain")
    @POST
    public String laskeJaSijoittele(List<LaskeDTO> lista);
}
