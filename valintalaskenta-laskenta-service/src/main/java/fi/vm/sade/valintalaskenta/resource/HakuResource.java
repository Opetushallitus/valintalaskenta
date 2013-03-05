package fi.vm.sade.valintalaskenta.resource;

import java.util.List;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import org.codehaus.jackson.map.annotate.JsonView;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import fi.vm.sade.service.valintaperusteet.model.JsonViews;
import fi.vm.sade.valintalaskenta.domain.Hakukohde;
import fi.vm.sade.valintalaskenta.service.ValintalaskentaTulosService;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
@Component
@Path("/haku")
public class HakuResource {

    protected static final Logger LOGGER = LoggerFactory.getLogger(HakuResource.class);

    @Autowired
    private ValintalaskentaTulosService tulosService;

    @GET
    @Produces(MediaType.APPLICATION_JSON)
    @JsonView({ JsonViews.Basic.class })
    public List<Hakukohde> haku() {
        return tulosService.haeHakukohteet();
    }

    @GET
    @Produces(MediaType.APPLICATION_JSON)
    @Path("{hakuoid}/hakukohde")
    @JsonView({ JsonViews.Basic.class })
    public List<Hakukohde> haku(@PathParam("hakuoid") String hakuoid) {
        return tulosService.haeHakukohteetHaulle(hakuoid);
    }
}
