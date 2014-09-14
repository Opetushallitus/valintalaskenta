package fi.vm.sade.valintalaskenta.tulos.resource.impl;

import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.READ_UPDATE_CRUD;

import java.util.Arrays;
import java.util.List;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.ValintatietoValinnanvaiheDTO;
import org.codehaus.jackson.map.annotate.JsonView;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Component;

import com.wordnik.swagger.annotations.Api;
import com.wordnik.swagger.annotations.ApiOperation;
import com.wordnik.swagger.annotations.ApiParam;

import fi.vm.sade.valintalaskenta.domain.JsonViews;
import fi.vm.sade.valintalaskenta.domain.dto.ValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.tulos.resource.HakukohdeResource;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;

/**
 * @author Jussi Jartamo
 */
@Component
@Path("hakukohde")
@PreAuthorize("isAuthenticated()")
@Api(value = "/hakukohde", description = "Resurssi tulosten hakemiseen hakukohteittain")
public class HakukohdeResourceImpl implements HakukohdeResource {

	protected static final Logger LOGGER = LoggerFactory
			.getLogger(HakukohdeResourceImpl.class);

	@Autowired
	private ValintalaskentaTulosService tulosService;

	@GET
	@Path("{hakukohdeoid}/valinnanvaihe")
	@Produces(MediaType.APPLICATION_JSON)
	@JsonView({ JsonViews.Basic.class })
	@PreAuthorize(READ_UPDATE_CRUD)
	@ApiOperation(value = "Hakee hakukohteen valinnan vaiheiden tulokset", response = ValinnanvaiheDTO.class)
	public List<ValintatietoValinnanvaiheDTO> hakukohde(
			@ApiParam(value = "Hakukohteen OID", required = true) @PathParam("hakukohdeoid") String hakukohdeoid) {
		return tulosService.haeValinnanvaiheetHakukohteelle(hakukohdeoid);
	}

	@POST
	@Path("{hakukohdeoid}/valinnanvaihe")
	@Produces(MediaType.APPLICATION_JSON)
	@Consumes(MediaType.APPLICATION_JSON)
	@JsonView({ JsonViews.Basic.class })
	@ApiOperation(value = "Lisää tuloksia valinnanvaiheelle", response = ValinnanvaiheDTO.class)
	public Response lisaaTuloksia(
			@ApiParam(value = "Hakukohteen OID", required = true) @PathParam("hakukohdeoid") String hakukohdeoid,
            @ApiParam(value = "Tarjoaja OID", required = true) @QueryParam("tarjoajaOid") String tarjoajaOid,
			@ApiParam(value = "Muokattava valinnanvaihe", required = true) ValinnanvaiheDTO vaihe) {
		try {
			ValinnanvaiheDTO vastaus = tulosService.lisaaTuloksia(vaihe,
					hakukohdeoid, tarjoajaOid);
			return Response.status(Response.Status.ACCEPTED).entity(vastaus)
					.build();
		} catch (Exception e) {
			LOGGER.error(
					"Valintatapajonon pisteitä ei saatu päivitettyä hakukohteelle {}, {}\r\n{}\r\n{}",
					hakukohdeoid, e.getMessage(),
					Arrays.toString(e.getStackTrace()));
			// , new GsonBuilder()
			// .setPrettyPrinting().create().toJson(vaihe)
			return Response.status(Response.Status.INTERNAL_SERVER_ERROR)
					.build();
		}
	}
}
