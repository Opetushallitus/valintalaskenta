package fi.vm.sade.valintalaskenta.resource;

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
import fi.vm.sade.valintalaskenta.domain.Hakukohde;
import fi.vm.sade.valintalaskenta.domain.VersiohallintaHakukohde;

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
    private Datastore datastore;

    @GET
    @Produces(MediaType.APPLICATION_JSON)
    @JsonView({ JsonViews.Basic.class })
    public List<Hakukohde> haku() {
        List<VersiohallintaHakukohde> versiohallinta = datastore.find(VersiohallintaHakukohde.class).asList();
        return Lists.newArrayList(Iterables.transform(versiohallinta,
                new Function<VersiohallintaHakukohde, Hakukohde>() {
                    public Hakukohde apply(@Nonnull VersiohallintaHakukohde input) {
                        assert (!input.getHakukohteet().isEmpty());
                        return input.getHakukohteet().last().getHakukohde();
                    }
                }));
    }

    @GET
    @Produces(MediaType.APPLICATION_JSON)
    @Path("{hakuoid}/hakukohde")
    @JsonView({ JsonViews.Basic.class })
    public List<Hakukohde> haku(@PathParam("hakuoid") String hakuoid) {
        List<VersiohallintaHakukohde> versiohallinta = datastore
                .find(VersiohallintaHakukohde.class, "hakuoid", hakuoid).asList();
        return Lists.newArrayList(Iterables.transform(versiohallinta,
                new Function<VersiohallintaHakukohde, Hakukohde>() {
                    public Hakukohde apply(@Nonnull VersiohallintaHakukohde input) {
                        assert (!input.getHakukohteet().isEmpty());
                        return input.getHakukohteet().last().getHakukohde();
                    }
                }));
    }
}