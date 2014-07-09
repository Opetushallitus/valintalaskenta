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

import fi.vm.sade.valintalaskenta.domain.JsonViews;
import fi.vm.sade.valintalaskenta.domain.dto.HakukohdeDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.ValintakoeOsallistuminenDTO;

/**
 * @author Jussi Jartamo
 */
@Path("haku")
@Api(value = "/haku", description = "Resurssi haun valintalaskennan virhetilanteiden hakemiseen")
public interface HakuResource {

	@GET
	@Path("{hakuOid}/virheet")
	@Produces(MediaType.APPLICATION_JSON)
	@JsonView({ JsonViews.Basic.class })
	@ApiOperation(value = "Hakee haun valintalaskennan virhetilanteet OID:n perusteella", response = HakukohdeDTO.class)
	public List<HakukohdeDTO> virheet(@PathParam("hakuOid") String hakuOid);

	@GET
	@Path("{hakuOid}/valintakoevirheet")
	@Produces(MediaType.APPLICATION_JSON)
	@JsonView(JsonViews.Basic.class)
	public List<ValintakoeOsallistuminenDTO> valintakoevirheet(
			@PathParam("hakuOid") String hakuOid);
}