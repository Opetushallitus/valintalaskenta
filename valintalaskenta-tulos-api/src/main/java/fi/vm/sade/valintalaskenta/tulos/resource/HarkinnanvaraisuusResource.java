package fi.vm.sade.valintalaskenta.tulos.resource;

import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;

import fi.vm.sade.valintalaskenta.domain.dto.HarkinnanvarainenHyvaksyminenDTO;

@Path("harkinnanvarainenhyvaksynta")
@Api(value = "/harkinnanvarainenhyvaksynta", description = "Resurssi harkinnanvaraisesti hakeneiden hakijoiden käsittelyyn")
public interface HarkinnanvaraisuusResource {
    @POST
    @Path("/haku/{hakuOid}/hakukohde/{hakukohdeOid}/hakemus/{hakemusOid}")
    @Produces(MediaType.APPLICATION_JSON)
    @ApiOperation(value = "Asettaa tilan harkinnanvaraisesti hakeneelle hakijalle")
    public void asetaTila(
            @ApiParam(value = "Haun OID", required = true) @PathParam("hakuOid") String hakuOid,
            @ApiParam(value = "Hakukohteen OID", required = true) @PathParam("hakukohdeOid") String hakukohdeOid,
            @ApiParam(value = "Hakemuksen OID", required = true) @PathParam("hakemusOid") String hakemusOid,
            @ApiParam(value = "Asetettava tila", required = true) HarkinnanvarainenHyvaksyminenDTO harkinnanvarainenHyvaksyminen,
            @Context HttpServletRequest request);

    @POST
    @Produces(MediaType.APPLICATION_JSON)
    @ApiOperation(value = "Asettaa tilan harkinnanvaraisesti hakeneelle hakijalle")
    public void asetaTilat(
            @ApiParam(value = "Asetettava tila", required = true) List<HarkinnanvarainenHyvaksyminenDTO> harkinnanvaraisetHyvaksymiset,
            @Context HttpServletRequest request);

    @GET
    @Path("/haku/{hakuOid}/hakukohde/{hakukohdeOid}")
    @Produces(MediaType.APPLICATION_JSON)
    @ApiOperation(value = "Hakee hakukohteen harkinnanvaraisesti hakeneiden hakijoiden tilat", response = HarkinnanvarainenHyvaksyminenDTO.class)
    public List<HarkinnanvarainenHyvaksyminenDTO> hakukohde(
            @ApiParam(value = "Haku OID", required = true) @PathParam("hakuOid") String hakuOid,
            @ApiParam(value = "Hakukohde OID", required = true) @PathParam("hakukohdeOid") String hakukohdeOid);

    @GET
    @Path("/haku/{hakuOid}/hakemus/{hakemusOid}")
    @Produces(MediaType.APPLICATION_JSON)
    @ApiOperation(value = "Hakee hakemuksen harkinnanvaraisesti tilat", response = HarkinnanvarainenHyvaksyminenDTO.class)
    public List<HarkinnanvarainenHyvaksyminenDTO> hakemus(
            @ApiParam(value = "Haku OID", required = true) @PathParam("hakuOid") String hakuOid,
            @ApiParam(value = "Hakemus OID", required = true) @PathParam("hakemusOid") String hakemusOid);
}
