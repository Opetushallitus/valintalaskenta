package fi.vm.sade.valintalaskenta.tulos.resource.impl;

import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.OPH_CRUD;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import fi.vm.sade.valintalaskenta.domain.dto.ValintatapajonoDTO;
import fi.vm.sade.valintalaskenta.domain.valinta.Valintatapajono;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Component;

import com.wordnik.swagger.annotations.Api;
import com.wordnik.swagger.annotations.ApiOperation;
import com.wordnik.swagger.annotations.ApiParam;

import fi.vm.sade.valintalaskenta.domain.dto.MuokattuJonosijaArvoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.MuokattuJonosijaDTO;
import fi.vm.sade.valintalaskenta.domain.valinta.MuokattuJonosija;
import fi.vm.sade.valintalaskenta.tulos.mapping.ValintalaskentaModelMapper;
import fi.vm.sade.valintalaskenta.tulos.resource.ValintatapajonoResource;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;

import java.util.Optional;

/**
 * @author Jussi Jartamo
 */
@Component
@Path("valintatapajono")
@PreAuthorize("isAuthenticated()")
@Api(value = "/valintatapajono", description = "Resurssi valintatapajonon jonosijojen muokkaamiseen manuaalisesti")
public class ValintatapajonoResourceImpl implements ValintatapajonoResource {

	@Autowired
	private ValintalaskentaTulosService tulosService;

	@Autowired
	private ValintalaskentaModelMapper modelMapper;

	/**
	 * 
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
	@PreAuthorize(OPH_CRUD)
	@ApiOperation(value = "Muokkaa jonosijaa", response = MuokattuJonosijaDTO.class)
	public Response muutaJonosija(
			@ApiParam(value = "Valintatapajonon OID", required = true) @PathParam("valintatapajonoOid") String valintatapajonoOid,
			@ApiParam(value = "Hakemus OID", required = true) @PathParam("hakemusOid") String hakemusOid,
			@ApiParam(value = "Muokattavan järjestyskriteerin prioriteetti", required = true) @PathParam("jarjestyskriteeriPrioriteetti") Integer jarjestyskriteeriPrioriteetti,
			@ApiParam(value = "Järjestyskriteerin uusi arvo", required = true) MuokattuJonosijaArvoDTO arvo) {

		MuokattuJonosija muokattuJonosija = tulosService
				.muutaJarjestyskriteeri(valintatapajonoOid, hakemusOid,
						jarjestyskriteeriPrioriteetti, arvo);
		if (muokattuJonosija != null) {
			MuokattuJonosijaDTO map = modelMapper.map(muokattuJonosija,
					MuokattuJonosijaDTO.class);
			return Response.status(Response.Status.ACCEPTED).entity(map)
					.build();
		} else {
			return Response.status(Response.Status.NOT_FOUND).build();
		}
	}

    @PUT
    @Consumes(MediaType.APPLICATION_JSON)
    @Produces(MediaType.APPLICATION_JSON)
    @Path("{valintatapajonoOid}/valmissijoiteltavaksi")
    @ApiOperation(value = "Lisää/Poistaa valintatapajonon sijoittelusta", response = ValintatapajonoDTO.class)
    public Response muokkaaSijotteluStatusta(@ApiParam(value = "Valintatapajonon OID", required = true) @PathParam("valintatapajonoOid") String valintatapajonoOid,
                                             @ApiParam(value = "Sijoittelustatus", required = true) @QueryParam("status") boolean status) {
        Optional<Valintatapajono> dto = tulosService.muokkaaSijotteluStatusta(valintatapajonoOid, status);

        return dto.map(jono -> Response.status(Response.Status.ACCEPTED).entity(modelMapper.map(jono, ValintatapajonoDTO.class)).build()).orElse(Response.status(Response.Status.NOT_FOUND).build());

    }
}
