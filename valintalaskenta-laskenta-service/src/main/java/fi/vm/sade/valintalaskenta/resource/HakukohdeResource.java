package fi.vm.sade.valintalaskenta.resource;

import java.util.Collections;
import java.util.List;

import javax.annotation.Nonnull;
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

import com.google.code.morphia.Datastore;
import com.google.common.base.Function;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;

import fi.vm.sade.service.valintaperusteet.model.JsonViews;
import fi.vm.sade.valintalaskenta.domain.Valinnanvaihe;
import fi.vm.sade.valintalaskenta.domain.VersiohallintaHakukohde;

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
    private Datastore datastore;

    @GET
    @Path("{hakukohdeoid}/valinnanvaihe")
    @Produces(MediaType.APPLICATION_JSON)
    @JsonView({ JsonViews.Basic.class })
    public List<Valinnanvaihe> hakukohde(@PathParam("hakukohdeoid") String hakukohdeoid) {
        List<VersiohallintaHakukohde> versiohallinta = datastore.find(VersiohallintaHakukohde.class, "hakukohdeoid",
                hakukohdeoid).asList();
        if (versiohallinta == null || versiohallinta.size() == 0) {
            LOGGER.debug("Hakukohteita oid:llä '{}' ei löytynyt! Annetaan palautteena tyhjä lista!", hakukohdeoid);
            return Collections.emptyList();
        }
        return Lists.newArrayList(Iterables.transform(versiohallinta,
                new Function<VersiohallintaHakukohde, Valinnanvaihe>() {
                    public Valinnanvaihe apply(@Nonnull VersiohallintaHakukohde input) {
                        assert (!input.getHakukohteet().isEmpty());
                        return input.getHakukohteet().last().getHakukohde().getValinnanvaihe();
                    }
                }));
    }
}
