package fi.vm.sade.valintalaskenta.tulos.resource;

import java.util.List;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.ValintatietoValinnanvaiheDTO;
import org.codehaus.jackson.map.annotate.JsonView;

import com.wordnik.swagger.annotations.Api;
import com.wordnik.swagger.annotations.ApiOperation;
import com.wordnik.swagger.annotations.ApiParam;

import fi.vm.sade.valintalaskenta.domain.JsonViews;
import fi.vm.sade.valintalaskenta.domain.dto.ValinnanvaiheDTO;

/**
 * @author Jussi Jartamo
 */
@Path("hakukohde")
@Api(value = "/hakukohde", description = "Resurssi tulosten hakemiseen hakukohteittain")
public interface HakukohdeResource {

	@GET
	@Path("{hakukohdeoid}/valinnanvaihe")
	@Produces(MediaType.APPLICATION_JSON)
	@JsonView({ JsonViews.Basic.class })
	@ApiOperation(value = "Hakee hakukohteen valinnan vaiheiden tulokset", response = ValinnanvaiheDTO.class)
	public List<ValintatietoValinnanvaiheDTO> hakukohde(
			@ApiParam(value = "Hakukohteen OID", required = true) @PathParam("hakukohdeoid") String hakukohdeoid);

    @POST
    @Path("{hakukohdeoid}/valinnanvaihe")
    @Produces(MediaType.APPLICATION_JSON)
    @Consumes(MediaType.APPLICATION_JSON)
    @JsonView({ JsonViews.Basic.class })
    @ApiOperation(value = "Lisää tuloksia valinnanvaiheelle", response = ValinnanvaiheDTO.class)
    public Response lisaaTuloksia(
            @ApiParam(value = "Hakukohteen OID", required = true) @PathParam("hakukohdeoid") String hakukohdeoid,
            @ApiParam(value = "Tarjoaja OID", required = true) @QueryParam("tarjoajaOid") String tarjoajaOid,
            @ApiParam(value = "Muokattava valinnanvaihe", required = true) ValinnanvaiheDTO vaihe);
}
