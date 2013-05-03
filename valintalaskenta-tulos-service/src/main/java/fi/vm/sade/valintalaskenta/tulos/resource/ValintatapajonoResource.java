package fi.vm.sade.valintalaskenta.tulos.resource;

import java.util.List;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import org.codehaus.jackson.map.annotate.JsonView;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import fi.vm.sade.valintalaskenta.domain.Jarjestyskriteeritulos;
import fi.vm.sade.valintalaskenta.domain.JsonViews;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
@Component
@Path("/valintatapajono")
public class ValintatapajonoResource {

    @Autowired
    private ValintalaskentaTulosService tulosService;

    @GET
    @Path("{valintatapajonoid}/jarjestyskriteeritulos")
    @Produces(MediaType.APPLICATION_JSON)
    @JsonView({ JsonViews.Basic.class })
    public List<Jarjestyskriteeritulos> valinnanvaihe(@PathParam("valintatapajonoid") String valintatapajonoid) {
        // kaikki versiot tästä valintatapajonosta
        return tulosService.haeJarjestyskriteerituloksetValintatapajonolle(valintatapajonoid);
    }

}
