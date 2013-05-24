package fi.vm.sade.valintalaskenta.tulos.resource;

import fi.vm.sade.valintalaskenta.domain.JsonViews;
import fi.vm.sade.valintalaskenta.domain.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import org.codehaus.jackson.map.annotate.JsonView;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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
@Path("/hakukohde")
public class HakukohdeResource {

    protected static final Logger LOGGER = LoggerFactory.getLogger(HakukohdeResource.class);

    @Autowired
    private ValintalaskentaTulosService tulosService;

    @GET
    @Path("{hakukohdeoid}/valinnanvaihe")
    @Produces(MediaType.APPLICATION_JSON)
    @JsonView({JsonViews.Basic.class})
    public List<Valinnanvaihe> hakukohde(@PathParam("hakukohdeoid") String hakukohdeoid) {

        List<Valinnanvaihe> tulos = tulosService.haeValinnanvaiheetHakukohteelle(hakukohdeoid);
        return tulos;
    }
}
