package fi.vm.sade.valintalaskenta.tulos.resource;

import fi.vm.sade.valintalaskenta.domain.JsonViews;
import fi.vm.sade.valintalaskenta.domain.Valintatapajono;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import org.codehaus.jackson.map.annotate.JsonView;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import java.util.List;

/**
 * @author Jussi Jartamo
 */
@Component
@Path("/valinnanvaihe")
public class ValinnanvaiheResource {

    @Autowired
    private ValintalaskentaTulosService tulosService;

    @GET
    @Path("{valinnanvaiheoid}/valintatapajono")
    @Produces(MediaType.APPLICATION_JSON)
    @JsonView({JsonViews.Basic.class})
    public List<Valintatapajono> valinnanvaihe(@PathParam("valinnanvaiheoid") String valinnanvaiheoid) {
        List<Valintatapajono> tulos = tulosService.haeValintatapajonoValinnanvaiheelle(valinnanvaiheoid);
        return tulos;
    }
}
