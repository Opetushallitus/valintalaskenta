package fi.vm.sade.valintalaskenta.tulos.resource.impl;

import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.READ_UPDATE_CRUD;

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
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Component;

import com.wordnik.swagger.annotations.Api;
import com.wordnik.swagger.annotations.ApiOperation;

import fi.vm.sade.valintalaskenta.domain.JsonViews;
import fi.vm.sade.valintalaskenta.domain.dto.HakukohdeDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.ValintakoeOsallistuminenDTO;
import fi.vm.sade.valintalaskenta.tulos.resource.HakuResource;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;

/**
 * @author Jussi Jartamo
 */
@Component
@Path("haku")
@PreAuthorize("isAuthenticated()")
@Api(value = "/haku", description = "Resurssi haun valintalaskennan virhetilanteiden hakemiseen")
public class HakuResourceImpl implements HakuResource {

	protected static final Logger LOGGER = LoggerFactory
			.getLogger(HakuResourceImpl.class);

	@Autowired
	private ValintalaskentaTulosService tulosService;

	@GET
	@Path("{hakuOid}/virheet")
	@Produces(MediaType.APPLICATION_JSON)
	@JsonView({ JsonViews.Basic.class })
	@PreAuthorize(READ_UPDATE_CRUD)
	@ApiOperation(value = "Hakee haun valintalaskennan virhetilanteet OID:n perusteella", response = HakukohdeDTO.class)
	public List<HakukohdeDTO> virheet(@PathParam("hakuOid") String hakuOid) {
		return tulosService.haeVirheetHaulle(hakuOid);
	}

	@GET
	@Path("{hakuOid}/valintakoevirheet")
	@Produces(MediaType.APPLICATION_JSON)
	@JsonView(JsonViews.Basic.class)
	@PreAuthorize(READ_UPDATE_CRUD)
	public List<ValintakoeOsallistuminenDTO> valintakoevirheet(
			@PathParam("hakuOid") String hakuOid) {
		return tulosService.haeValintakoevirheetHaulle(hakuOid);
	}
}
