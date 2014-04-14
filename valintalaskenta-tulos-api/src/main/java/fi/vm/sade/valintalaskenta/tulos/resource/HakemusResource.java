package fi.vm.sade.valintalaskenta.tulos.resource;

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
import fi.vm.sade.valintalaskenta.domain.dto.HakemusDTO;

/**
 * @author Jussi Jartamo
 */
@Path("hakemus")
@Api(value = "/hakemus", description = "Resurssi hakemuskohtaisten tulosten hakemiseen")
public interface HakemusResource {

	@GET
	@Path("{hakuoid}/{hakemusoid}")
	@Produces(MediaType.APPLICATION_JSON)
	@JsonView({ JsonViews.Basic.class })
	@ApiOperation(value = "Hakee hakemuksen tulokset haku OID:n ja hakemuksen OID:n perustella", response = HakemusDTO.class)
	public HakemusDTO hakemus(
			@ApiParam(value = "Haku OID", required = true) @PathParam("hakuoid") String hakuoid,
			@ApiParam(value = "Hakemus OID", required = true) @PathParam("hakemusoid") String hakemusoid);
}
