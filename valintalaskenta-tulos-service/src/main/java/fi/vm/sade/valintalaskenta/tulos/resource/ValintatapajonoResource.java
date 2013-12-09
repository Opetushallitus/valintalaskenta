package fi.vm.sade.valintalaskenta.tulos.resource;

import com.wordnik.swagger.annotations.Api;
import com.wordnik.swagger.annotations.ApiOperation;
import com.wordnik.swagger.annotations.ApiParam;
import fi.vm.sade.valintalaskenta.domain.dto.MuokattuJonosijaArvoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.MuokattuJonosijaDTO;
import fi.vm.sade.valintalaskenta.tulos.mapping.ValintalaskentaModelMapper;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.annotation.Secured;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Component;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;

import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.OPH_CRUD;

/**
 * @author Jussi Jartamo
 */
@Component
@Path("valintatapajono")
@PreAuthorize("isAuthenticated()")
@Api(value = "/valintatapajono", description = "Resurssi valintatapajonon jonosijojen muokkaamiseen manuaalisesti")
public class ValintatapajonoResource {

    @Autowired
    private ValintalaskentaTulosService tulosService;

    @Autowired
    private ValintalaskentaModelMapper modelMapper;

    /**
     * @param valintatapajonoOid
     * @param hakemusOid
     * @param jarjestyskriteeriPrioriteetti
     * @param arvo
     * @return
     */
    @POST
    @Consumes(MediaType.APPLICATION_JSON)
    @Produces(MediaType.APPLICATION_JSON)
    @Path("{valintatapajonoOid}/{hakemusOid}/{jarjestyskriteeriPrioriteetti}/jonosija")
    @Secured({OPH_CRUD})
    @ApiOperation(value = "Muokkaa jonosijaa", response = MuokattuJonosijaDTO.class)
    public MuokattuJonosijaDTO muutaJonosija(
            @ApiParam(value = "Valintatapajonon OID", required = true) @PathParam("valintatapajonoOid") String valintatapajonoOid,
            @ApiParam(value = "Hakemus OID", required = true) @PathParam("hakemusOid") String hakemusOid,
            @ApiParam(value = "Muokattavan järjestyskriteerin prioriteetti", required = true) @PathParam("jarjestyskriteeriPrioriteetti") Integer jarjestyskriteeriPrioriteetti,
            @ApiParam(value = "Seliteteksti", required = true) @QueryParam("selite") String selite,
            @ApiParam(value = "Järjestyskriteerin uusi arvo", required = true) MuokattuJonosijaArvoDTO arvo) {
        return modelMapper.map(tulosService.muutaJarjestyskriteeri(valintatapajonoOid,
                hakemusOid,
                jarjestyskriteeriPrioriteetti,
                arvo, selite), MuokattuJonosijaDTO.class);
    }
}
