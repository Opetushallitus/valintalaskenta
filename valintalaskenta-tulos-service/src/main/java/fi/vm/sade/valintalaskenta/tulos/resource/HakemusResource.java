package fi.vm.sade.valintalaskenta.tulos.resource;

import fi.vm.sade.valintalaskenta.domain.JsonViews;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;
import fi.vm.sade.valintalaskenta.domain.valinta.JarjestyskriteerituloksenTila;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import org.codehaus.jackson.map.annotate.JsonView;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.annotation.Secured;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Component;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;

import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.*;

/**
 *
 * @author Jussi Jartamo
 *
 */
@Component
@Path("hakemus")
@PreAuthorize("isAuthenticated()")
public class HakemusResource {

    @Autowired
    private ValintalaskentaTulosService tulosService;

    @GET
    @Path("{hakuoid}/{hakemusoid}")
    @Produces(MediaType.APPLICATION_JSON)
    @JsonView({ JsonViews.Basic.class })
    @Secured({ READ, UPDATE, CRUD })
    public HakemusDTO hakemus(@PathParam("hakuoid") String hakuoid,
                              @PathParam("hakemusoid") String hakemusoid) {
        return tulosService.haeTuloksetHakemukselle(hakuoid, hakemusoid);
    }


    @POST
    @Path("{hakuoid}/{hakemusoid}/harkinnanvarainen/{hakukohdeoid}")
    @Produces(MediaType.APPLICATION_JSON)
    @JsonView({ JsonViews.Basic.class })
    @Secured({ UPDATE, CRUD })
    public HakemusDTO harkinnanvaraisuusPost(@PathParam("hakuoid") String hakuoid,
                                             @PathParam("hakemusoid") String hakemusoid,
                                             @PathParam("hakukohdeoid") String hakukohdeoid,
                                             JarjestyskriteerituloksenTila tila) {
        return tulosService.asetaHarkinnanvaraisestiHyvaksymisenTila(hakuoid, hakukohdeoid, hakemusoid, tila);
    }

    /*
    @GET
    @Path("{hakuoid}/{hakemusoid}/harkinnanvarainen/{hakukohdeoid}")
    @Produces(MediaType.APPLICATION_JSON)
    @JsonView({ JsonViews.Basic.class })
    @Secured({ READ, UPDATE, CRUD })
    public HakemusDTO harkinnanvaraisuus(@PathParam("hakuoid") String hakuoid,
                                         @PathParam("hakemusoid") String hakemusoid,
                                         @PathParam("hakukohdeoid") String hakukohdeoid,
                                         JarjestyskriteerituloksenTila tila) {
        return tulosService.haeTuloksetHakemukselle(hakuoid, hakemusoid);
    }
         */
}
