package fi.vm.sade.service.valintalaskenta.resource;

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

import fi.vm.sade.service.valintaperusteet.algoritmi.domain.Hakukohde;
import fi.vm.sade.service.valintaperusteet.algoritmi.domain.VersiohallintaHakukohde;
import fi.vm.sade.service.valintaperusteet.model.JsonViews;

/**
 * 
 * @author Jussi Jartamo
 * 
 */
@Component
@Path("/hakukohde")
public class HakukohdeResource {

    protected final static Logger LOGGER = LoggerFactory.getLogger(HakukohdeResource.class);

    @Autowired
    private Datastore datastore;

    @GET
    @Path("{hakukohdeoid}/valinnanvaihe")
    @Produces(MediaType.APPLICATION_JSON)
    @JsonView({ JsonViews.Basic.class })
    public List<Hakukohde> hakukohde(@PathParam("hakukohdeoid") String hakukohdeoid) {
        List<VersiohallintaHakukohde> versiohallinta = datastore.find(VersiohallintaHakukohde.class, "hakukohdeoid",
                hakukohdeoid).asList();
        if (versiohallinta == null || versiohallinta.size() == 0) {
            LOGGER.error("Hakukohdetta oid:llä '{}' ei löytynyt! Annetaan palautteena tyhjäviitevastaus!", hakukohdeoid);
            return null;
        }
        return Lists.newArrayList(Iterables.transform(versiohallinta,
                new Function<VersiohallintaHakukohde, Hakukohde>() {
                    public Hakukohde apply(@Nonnull VersiohallintaHakukohde input) {
                        assert (input.getHakukohteet().isEmpty() != true);
                        return input.getHakukohteet().last().getHakukohde();
                    }
                }));
    }
}
