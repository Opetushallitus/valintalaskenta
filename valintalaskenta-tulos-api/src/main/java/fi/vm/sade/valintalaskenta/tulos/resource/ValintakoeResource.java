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
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.ValintakoeOsallistuminenDTO;

@Path("valintakoe")
@Api(value = "/valintakoe", description = "Resurssi valintakoeosallistumistulosten hakemiseen")
public interface ValintakoeResource {

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("hakemus/{hakemusOid}")
	@JsonView({ JsonViews.Basic.class })
	@ApiOperation(value = "Hakee valintakoeosallistumiset hakemukselle OID:n perusteella", response = ValintakoeOsallistuminenDTO.class)
	public ValintakoeOsallistuminenDTO haku(
			@ApiParam(value = "Hakemus OID", required = true) @PathParam("hakemusOid") String hakemusOid);

	@GET
	@Produces(MediaType.APPLICATION_JSON)
	@Path("hakutoive/{hakukohdeOid}")
	@JsonView({ JsonViews.Basic.class })
	@ApiOperation(value = "Hakee valintakoeosallistumiset hakukohteelle OID:n perusteella", response = ValintakoeOsallistuminenDTO.class)
	public List<ValintakoeOsallistuminenDTO> hakuByHakutoive(
			@ApiParam(value = "Hakukohde OID", required = true) @PathParam("hakukohdeOid") String hakukohdeOid);

}
