package fi.vm.sade.valintalaskenta.tulos.resource;

import com.wordnik.swagger.annotations.Api;
import com.wordnik.swagger.annotations.ApiOperation;
import fi.vm.sade.valintalaskenta.domain.JsonViews;
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import org.codehaus.jackson.map.annotate.JsonView;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.annotation.Secured;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Component;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.*;

/**
 * @author Jussi Jartamo
 */
@Component
@Path("hakemus")
@PreAuthorize("isAuthenticated()")
@Api(value = "/hakemus", description = "Resurssi hakemuskohtaisten tulosten hakemiseen")
public class HakemusResource {

    @Autowired
    private ValintalaskentaTulosService tulosService;

    @GET
    @Path("{hakuoid}/{hakemusoid}")
    @Produces(MediaType.APPLICATION_JSON)
    @JsonView({JsonViews.Basic.class})
    @Secured({READ, UPDATE, CRUD})
    @ApiOperation(value = "Hakee hakemuksen tulokset haku OID:n ja hakemuksen OID:n perustella", response = HakemusDTO.class)
    public HakemusDTO hakemus(@PathParam("hakuoid") String hakuoid,
                              @PathParam("hakemusoid") String hakemusoid) {
        return tulosService.haeTuloksetHakemukselle(hakuoid, hakemusoid);
    }
}
