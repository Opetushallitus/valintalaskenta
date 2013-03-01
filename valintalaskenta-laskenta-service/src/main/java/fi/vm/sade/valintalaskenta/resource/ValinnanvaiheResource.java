package fi.vm.sade.valintalaskenta.resource;

import java.util.Collections;
import java.util.List;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import org.codehaus.jackson.map.annotate.JsonView;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.google.code.morphia.Datastore;

import fi.vm.sade.service.valintaperusteet.model.JsonViews;
import fi.vm.sade.valintalaskenta.domain.Valintatapajono;
import fi.vm.sade.valintalaskenta.domain.VersiohallintaHakukohde;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
@Component
@Path("/valinnanvaihe")
public class ValinnanvaiheResource {

    @Autowired
    private Datastore datastore;

    @GET
    @Path("{valinnanvaiheoid}/valintatapajono")
    @Produces(MediaType.APPLICATION_JSON)
    @JsonView({ JsonViews.Basic.class })
    public List<Valintatapajono> valinnanvaihe(@PathParam("valinnanvaiheoid") String valinnanvaiheoid) {
        VersiohallintaHakukohde versiohallinta = datastore.find(VersiohallintaHakukohde.class, "valinnanvaiheoid",
                valinnanvaiheoid).get();
        if (versiohallinta == null) {
            return Collections.emptyList();
        }
        return versiohallinta.getHakukohteet().last().getHakukohde().getValinnanvaihe().getValintatapajono();
    }
}
