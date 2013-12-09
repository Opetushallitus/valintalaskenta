package fi.vm.sade.valintalaskenta.tulos.resource;

import com.wordnik.swagger.annotations.Api;
import com.wordnik.swagger.annotations.ApiOperation;
import com.wordnik.swagger.annotations.ApiParam;
import fi.vm.sade.valintalaskenta.domain.JsonViews;
import fi.vm.sade.valintalaskenta.domain.dto.HarkinnanvarainenHyvaksyminenDTO;
import fi.vm.sade.valintalaskenta.tulos.mapping.ValintalaskentaModelMapper;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import org.codehaus.jackson.map.annotate.JsonView;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.annotation.Secured;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Component;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import java.util.List;

import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.*;

/**
 * @author Jussi Jartamo
 */
@Component
@Path("harkinnanvarainenhyvaksynta")
@PreAuthorize("isAuthenticated()")
@Api(value = "/harkinnanvarainenhyvaksynta", description = "Resurssi harkinnanvaraisesti hakeneiden hakijoiden käsittelyyn")
public class HarkinnanvaraisuusResource {

    @Autowired
    private ValintalaskentaTulosService tulosService;

    @Autowired
    private ValintalaskentaModelMapper modelMapper;

    @POST
    @Path("/haku/{hakuOid}/hakukohde/{hakukohdeOid}/hakemus/{hakemusOid}")
    @Produces(MediaType.APPLICATION_JSON)
    @JsonView({JsonViews.Basic.class})
    @Secured({UPDATE, CRUD})
    @ApiOperation(value = "Asettaa tilan harkinnanvaraisesti hakeneelle hakijalle")
    public void hakemus(@ApiParam(value = "Haun OID", required = true) @PathParam("hakuOid") String hakuOid,
                        @ApiParam(value = "Hakukohteen OID", required = true) @PathParam("hakukohdeOid") String hakukohdeOid,
                        @ApiParam(value = "Hakemuksen OID", required = true) @PathParam("hakemusOid") String hakemusOid,
                        @ApiParam(value = "Asetettava tila", required = true) HarkinnanvarainenHyvaksyminenDTO harkinnanvarainenHyvaksyminen) {
        tulosService.asetaHarkinnanvaraisestiHyvaksymisenTila(hakuOid, hakukohdeOid, hakemusOid, harkinnanvarainenHyvaksyminen.getHarkinnanvaraisuusTila());
    }

    @GET
    @Path("/haku/{hakuOid}/hakukohde/{hakukohdeOid}")
    @Produces(MediaType.APPLICATION_JSON)
    @JsonView({JsonViews.Basic.class})
    @Secured({READ, UPDATE, CRUD})
    @ApiOperation(value = "Hakee hakukohteen harkinnanvaraisesti hakeneiden hakijoiden tilat", response = HarkinnanvarainenHyvaksyminenDTO.class)
    public List<HarkinnanvarainenHyvaksyminenDTO> hakemus(@ApiParam(value = "Haku OID", required = true) @PathParam("hakuOid") String hakuOid,
                                                          @ApiParam(value = "Hakukohde OID", required = true) @PathParam("hakukohdeOid") String hakukohdeOid) {
        return modelMapper.mapList(tulosService.haeHarkinnanvaraisestiHyvaksymisenTila(hakukohdeOid), HarkinnanvarainenHyvaksyminenDTO.class);
    }
}
