package fi.vm.sade.valintalaskenta.tulos.resource;

import java.util.List;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import fi.vm.sade.valintalaskenta.domain.Jonosija;
import fi.vm.sade.valintalaskenta.domain.Valintatapajono;
import org.codehaus.jackson.map.annotate.JsonView;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import fi.vm.sade.valintalaskenta.domain.JsonViews;
import fi.vm.sade.valintalaskenta.domain.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;

/**
 *
 * @author Jussi Jartamo
 *
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
    @JsonView({ JsonViews.Basic.class })
    public List<Valinnanvaihe> hakukohde(@PathParam("hakukohdeoid") String hakukohdeoid) {

        List<Valinnanvaihe> tulos = tulosService.haeValinnanvaiheetHakukohteelle(hakukohdeoid);

        for(Valinnanvaihe valinnanvaihe : tulos) {
            System.out.println("valinnanvaihe");
            for(Valintatapajono valintatapajono : valinnanvaihe.getValintatapajono()) {
                System.out.println("JONOSIJAT SIZE: " + valintatapajono.getJonosijat().size());
                for(Jonosija s : valintatapajono.getJonosijat()) {
                    System.out.println( s.getJonosija() + " " + s.getHakemusoid()) ;
                }
            }
        }
        return tulos;
    }
}
