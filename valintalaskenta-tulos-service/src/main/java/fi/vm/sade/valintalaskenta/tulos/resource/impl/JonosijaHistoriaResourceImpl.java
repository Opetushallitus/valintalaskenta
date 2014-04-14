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
import com.wordnik.swagger.annotations.ApiParam;

import fi.vm.sade.valintalaskenta.domain.JsonViews;
import fi.vm.sade.valintalaskenta.domain.dto.JarjestyskriteerihistoriaDTO;
import fi.vm.sade.valintalaskenta.tulos.mapping.ValintalaskentaModelMapper;
import fi.vm.sade.valintalaskenta.tulos.resource.JonosijaHistoriaResource;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;

/**
 * User: tommiha Date: 8/9/13 Time: 10:56 AM
 */
@Component
@Path("jonosijahistoria")
@PreAuthorize("isAuthenticated()")
@Api(value = "/jonosijahistoria", description = "Resurssi jonosijahistoriatiedon hakemiseen")
public class JonosijaHistoriaResourceImpl implements JonosijaHistoriaResource {

	protected static final Logger logger = LoggerFactory
			.getLogger(JonosijaHistoriaResourceImpl.class);

	@Autowired
	private ValintalaskentaTulosService tulosService;

	@Autowired
	private ValintalaskentaModelMapper modelMapper;

	@GET
	@Path("{valintatapajonoOid}/{hakemusOid}")
	@Produces(MediaType.APPLICATION_JSON)
	@JsonView({ JsonViews.Basic.class })
	@PreAuthorize(READ_UPDATE_CRUD)
	@ApiOperation(value = "Hakee jonosijahistoriat valintatapajono OID:n ja hakemus OID:n perusteella", response = JarjestyskriteerihistoriaDTO.class)
	public List<JarjestyskriteerihistoriaDTO> listJonosijaHistoria(
			@ApiParam(value = "Valintatapajono OID", required = true) @PathParam("valintatapajonoOid") String valintatapajonoOid,
			@ApiParam(value = "Hakemus OID", required = true) @PathParam("hakemusOid") String hakemusOid) {
		return modelMapper.mapList(tulosService.haeJonosijaHistoria(
				valintatapajonoOid, hakemusOid),
				JarjestyskriteerihistoriaDTO.class);
	}

}
