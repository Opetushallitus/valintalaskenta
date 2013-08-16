/*
package fi.vm.sade.valintalaskenta.tulos.resource;

import java.util.List;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import org.codehaus.jackson.map.annotate.JsonView;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.annotation.Secured;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Component;

import fi.vm.sade.valintalaskenta.domain.JsonViews;
import fi.vm.sade.valintalaskenta.domain.Versioituhakukohde;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;

import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.CRUD;
import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.READ;
import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.UPDATE;

**
 * 
 * @author Jussi Jartamo
 * 
 *
@Component
@Path("haku")
@PreAuthorize("isAuthenticated()")
public class HakuResource {

    protected static final Logger LOGGER = LoggerFactory.getLogger(HakuResource.class);

    @Autowired
    private ValintalaskentaTulosService tulosService;

    @GET
    @Produces(MediaType.APPLICATION_JSON)
    @JsonView({ JsonViews.Basic.class })
    @Secured({READ, UPDATE, CRUD})
    public List<Versioituhakukohde> haku() {
        return tulosService.haeHakukohteet();
    }

    @GET
    @Produces(MediaType.APPLICATION_JSON)
    @Path("{hakuoid}/hakukohde")
    @JsonView({ JsonViews.Basic.class })
    @Secured({READ, UPDATE, CRUD})
    public List<Versioituhakukohde> haku(@PathParam("hakuoid") String hakuoid) {
        return tulosService.haeHakukohteetHaulle(hakuoid);
    }

}
    */