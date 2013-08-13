package fi.vm.sade.valintalaskenta.tulos.resource;

import fi.vm.sade.valintalaskenta.domain.JonosijaHistoria;
import fi.vm.sade.valintalaskenta.domain.JsonViews;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import org.codehaus.jackson.map.annotate.JsonView;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.annotation.Secured;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Component;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import java.util.List;

import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.CRUD;
import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.READ;
import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.UPDATE;

/**
 * User: tommiha
 * Date: 8/9/13
 * Time: 10:56 AM
 */
@Component
@Path("jonosijahistoria")
@PreAuthorize("isAuthenticated()")
public class JonosijaHistoriaResource {

    protected static final Logger logger = LoggerFactory.getLogger(JonosijaHistoriaResource.class);

    @Autowired
    private ValintalaskentaTulosService tulosService;

    @GET
    @Path("{valintatapajonoOid}/{hakemusOid}")
    @Produces(MediaType.APPLICATION_JSON)
    @JsonView({JsonViews.Basic.class})
    @Secured({READ, UPDATE, CRUD})
    public List<JonosijaHistoria> listJonosijaHistoria(@PathParam("valintatapajonoOid") String valintatapajonoOid, @PathParam("hakemusOid") String hakemusOid) {
        return tulosService.haeJonosijaHistoria(valintatapajonoOid, hakemusOid);
    }

}
