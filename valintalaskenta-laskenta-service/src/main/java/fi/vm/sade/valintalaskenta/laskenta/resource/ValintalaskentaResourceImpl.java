package fi.vm.sade.valintalaskenta.laskenta.resource;

import fi.vm.sade.valintalaskenta.domain.dto.LaskeDTO;
import fi.vm.sade.valintalaskenta.domain.resource.ValintalaskentaResource;
import fi.vm.sade.valintalaskenta.laskenta.service.ValintalaskentaServiceRest;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Component;

import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;

/**
 * Created by jukais on 21.3.2014.
 */
@Component
@Path("valintalaskenta")
//@PreAuthorize("isAuthenticated()")
public class ValintalaskentaResourceImpl implements ValintalaskentaResource {

    @Autowired
    private ValintalaskentaServiceRest valintalaskentaService;

    @Override
    @Path("laske")
    @Consumes("application/json")
    @Produces("text/plain")
    @POST
    public String laske(LaskeDTO laskeDTO) {
        return valintalaskentaService.laske(laskeDTO.getHakemus(), laskeDTO.getValintaperuste());
    }

    @Override
    @Path("valintakokeet")
    @Consumes("application/json")
    @Produces("text/plain")
    @POST
    public String valintakokeet(LaskeDTO laskeDTO) {
        return valintalaskentaService.valintakokeet(laskeDTO.getHakemus().get(0),laskeDTO.getValintaperuste());
    }
}
