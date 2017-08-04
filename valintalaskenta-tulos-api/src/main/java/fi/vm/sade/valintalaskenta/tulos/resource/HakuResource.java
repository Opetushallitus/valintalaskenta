package fi.vm.sade.valintalaskenta.tulos.resource;

import java.util.List;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import fi.vm.sade.valintalaskenta.domain.dto.MinimalJonoDTO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;

import fi.vm.sade.valintalaskenta.domain.dto.HakukohdeDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.ValintakoeOsallistuminenDTO;

@Path("haku")
@Api(value = "/haku", description = "Resurssi haun valintalaskennan virhetilanteiden hakemiseen")
public interface HakuResource {

    @GET
    @Path("{hakuOid}/virheet")
    @Produces(MediaType.APPLICATION_JSON)
    @ApiOperation(value = "Hakee haun valintalaskennan virhetilanteet OID:n perusteella", response = HakukohdeDTO.class)
    List<HakukohdeDTO> virheet(@PathParam("hakuOid") String hakuOid);

    @GET
    @Path("{hakuOid}/valintakoevirheet")
    @Produces(MediaType.APPLICATION_JSON)
    List<ValintakoeOsallistuminenDTO> valintakoevirheet(@PathParam("hakuOid") String hakuOid);

    @GET
    @Path("/ilmanvalintalaskentaasijoitteluun")
    @Produces(MediaType.APPLICATION_JSON)
    @ApiOperation(value = "Hakee sijoitteluun siirretyt valintalaskennattomat valintatapajonot ODWlle", response = MinimalJonoDTO.class)
    List<MinimalJonoDTO> jonotSijoitteluun();

}
