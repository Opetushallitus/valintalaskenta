package fi.vm.sade.valintalaskenta.tulos.resource;

import java.util.List;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import org.codehaus.jackson.map.annotate.JsonView;

import com.wordnik.swagger.annotations.Api;
import com.wordnik.swagger.annotations.ApiOperation;
import com.wordnik.swagger.annotations.ApiParam;

import fi.vm.sade.valintalaskenta.domain.JsonViews;
import fi.vm.sade.valintalaskenta.domain.dto.JarjestyskriteerihistoriaDTO;

/**
 * User: tommiha Date: 8/9/13 Time: 10:56 AM
 */
@Path("jonosijahistoria")
@Api(value = "/jonosijahistoria", description = "Resurssi jonosijahistoriatiedon hakemiseen")
public interface JonosijaHistoriaResource {

	@GET
	@Path("{valintatapajonoOid}/{hakemusOid}")
	@Produces(MediaType.APPLICATION_JSON)
	@JsonView({ JsonViews.Basic.class })
	@ApiOperation(value = "Hakee jonosijahistoriat valintatapajono OID:n ja hakemus OID:n perusteella", response = JarjestyskriteerihistoriaDTO.class)
	public List<JarjestyskriteerihistoriaDTO> listJonosijaHistoria(
			@ApiParam(value = "Valintatapajono OID", required = true) @PathParam("valintatapajonoOid") String valintatapajonoOid,
			@ApiParam(value = "Hakemus OID", required = true) @PathParam("hakemusOid") String hakemusOid);

}
