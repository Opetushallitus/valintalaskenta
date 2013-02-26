package fi.vm.sade.service.valintalaskenta.resource;

import java.util.List;

import javax.annotation.Nullable;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
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
@Path("/haku")
public class HakuResource {

    protected final static Logger LOGGER = LoggerFactory.getLogger(HakuResource.class);

    @Autowired
    private Datastore datastore;

    @GET
    @Produces(MediaType.APPLICATION_JSON)
    @JsonView({ JsonViews.Basic.class })
    public List<Hakukohde> haku() {
        List<VersiohallintaHakukohde> versiohallinta = datastore.find(VersiohallintaHakukohde.class).asList();
        return Lists.newArrayList(Iterables.transform(versiohallinta,
                new Function<VersiohallintaHakukohde, Hakukohde>() {
                    public Hakukohde apply(@Nullable VersiohallintaHakukohde input) {
                        assert (input.getHakukohteet().isEmpty() != true);
                        return input.getHakukohteet().last().getHakukohde();
                    }
                }));
    }

}
