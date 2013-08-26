package fi.vm.sade.valintalaskenta.tulos.resource;

import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.CRUD;
import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.READ;
import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.UPDATE;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import org.codehaus.jackson.map.annotate.JsonView;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.annotation.Secured;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Component;

import fi.vm.sade.valintalaskenta.domain.JsonViews;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;

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
    @Path("{hakemusoid}")
    @Produces(MediaType.APPLICATION_JSON)
    @JsonView({ JsonViews.Basic.class })
    @Secured({ READ, UPDATE, CRUD })
    public HakemusDTO hakemus(@PathParam("hakuoid") String hakuoid, @PathParam("hakemusoid") String hakemusoid) {
        return tulosService.haeTuloksetHakemukselle(hakuoid, hakemusoid);
    }
}
