package fi.vm.sade.valintalaskenta.tulos.resource;

import com.wordnik.swagger.annotations.Api;
import com.wordnik.swagger.annotations.ApiOperation;
import com.wordnik.swagger.annotations.ApiParam;
import fi.vm.sade.valintalaskenta.domain.JsonViews;
import fi.vm.sade.valintalaskenta.domain.dto.ValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import org.codehaus.jackson.map.annotate.JsonView;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Component;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import java.util.List;

import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.*;

/**
 * @author Jussi Jartamo
 */
@Component
@Path("hakukohde")
@PreAuthorize("isAuthenticated()")
@Api(value = "/hakukohde", description = "Resurssi tulosten hakemiseen hakukohteittain")
public class HakukohdeResource {

    protected static final Logger LOGGER = LoggerFactory.getLogger(HakukohdeResource.class);

    @Autowired
    private ValintalaskentaTulosService tulosService;

    @GET
    @Path("{hakukohdeoid}/valinnanvaihe")
    @Produces(MediaType.APPLICATION_JSON)
    @JsonView({JsonViews.Basic.class})
    @PreAuthorize(READ_UPDATE_CRUD)
    @ApiOperation(value = "Hakee hakukohteen valinnan vaiheiden tulokset", response = ValinnanvaiheDTO.class)
    public List<ValinnanvaiheDTO> hakukohde(@ApiParam(value = "Hakukohteen OID", required = true) @PathParam("hakukohdeoid") String hakukohdeoid) {
        return tulosService.haeValinnanvaiheetHakukohteelle(hakukohdeoid);
    }
}
