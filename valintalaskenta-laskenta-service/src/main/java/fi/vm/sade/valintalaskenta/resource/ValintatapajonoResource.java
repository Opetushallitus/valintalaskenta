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
import com.google.common.collect.Sets;

import fi.vm.sade.service.valintaperusteet.model.JsonViews;
import fi.vm.sade.valintalaskenta.domain.Jarjestyskriteeritulos;
import fi.vm.sade.valintalaskenta.domain.Valintatapajono;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
@Component
@Path("/valintatapajono")
public class ValintatapajonoResource {

    @Autowired
    private Datastore datastore;

    @GET
    @Path("{valintatapajonoid}/jarjestyskriteeritulos")
    @Produces(MediaType.APPLICATION_JSON)
    @JsonView({ JsonViews.Basic.class })
    public List<Jarjestyskriteeritulos> valinnanvaihe(@PathParam("valintatapajonoid") String valintatapajonoid) {
        // kaikki versiot tästä valintatapajonosta
        Valintatapajono uusinvalintatapajono = Sets.newTreeSet(
                datastore.find(Valintatapajono.class, "valintatapajonooid", valintatapajonoid).asList()).last();

        return uusinvalintatapajono == null ? Collections.<Jarjestyskriteeritulos> emptyList() : uusinvalintatapajono
                .getJarjestyskriteeritulokset();
    }

}
