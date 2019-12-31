package fi.vm.sade.valintalaskenta.tulos.resource.impl;

import fi.vm.sade.auditlog.User;
import fi.vm.sade.service.valintaperusteet.dto.ValintatapajonoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.MuokattuJonosijaArvoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.MuokattuJonosijaDTO;
import fi.vm.sade.valintalaskenta.domain.valinta.MuokattuJonosija;
import fi.vm.sade.valintalaskenta.domain.valinta.Valintatapajono;
import fi.vm.sade.valintalaskenta.tulos.logging.LaskentaAuditLog;
import fi.vm.sade.valintalaskenta.tulos.mapping.ValintalaskentaModelMapper;
import fi.vm.sade.valintalaskenta.tulos.resource.ValintatapajonoResource;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.HashMap;
import java.util.Optional;
import java.util.function.Consumer;

import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.UPDATE_CRUD;

@Controller
@Path("valintatapajono")
@PreAuthorize("isAuthenticated()")
@Api(value = "/valintatapajono", description = "Resurssi valintatapajonon jonosijojen muokkaamiseen manuaalisesti")
public class ValintatapajonoResourceImpl implements ValintatapajonoResource {

    @Autowired
    private ValintalaskentaTulosService tulosService;

    @Autowired
    private ValintalaskentaModelMapper modelMapper;

    @Autowired
    private LaskentaAuditLog auditLog;

    @POST
    @Consumes(MediaType.APPLICATION_JSON)
    @Produces(MediaType.APPLICATION_JSON)
    @Path("{valintatapajonoOid}/{hakemusOid}/{jarjestyskriteeriPrioriteetti}/jonosija")
    @PreAuthorize(UPDATE_CRUD)
    @ApiOperation(value = "Muokkaa jonosijaa", response = MuokattuJonosijaDTO.class)
    public Response muutaJonosija(
            @ApiParam(value = "Valintatapajonon OID", required = true) @PathParam("valintatapajonoOid") String valintatapajonoOid,
            @ApiParam(value = "Hakemus OID", required = true) @PathParam("hakemusOid") String hakemusOid,
            @ApiParam(value = "Muokattavan järjestyskriteerin prioriteetti", required = true) @PathParam("jarjestyskriteeriPrioriteetti") Integer jarjestyskriteeriPrioriteetti,
            @ApiParam(value = "Järjestyskriteerin uusi arvo", required = true) MuokattuJonosijaArvoDTO arvo,
            HttpServletRequest request) {
        User user = auditLog.getUser(request);

        MuokattuJonosija muokattuJonosija = tulosService.muutaJarjestyskriteeri(valintatapajonoOid, hakemusOid, jarjestyskriteeriPrioriteetti, arvo, user);
        if (muokattuJonosija != null) {
            MuokattuJonosijaDTO map = modelMapper.map(muokattuJonosija, MuokattuJonosijaDTO.class);
            return Response.status(Response.Status.ACCEPTED).entity(map).build();
        } else {
            return Response.status(Response.Status.NOT_FOUND).build();
        }
    }

    @DELETE
    @Consumes(MediaType.APPLICATION_JSON)
    @Produces(MediaType.APPLICATION_JSON)
    @Path("{valintatapajonoOid}/{hakemusOid}/{jarjestyskriteeriPrioriteetti}/jonosija")
    @PreAuthorize(UPDATE_CRUD)
    @ApiOperation(value = "Muokkaa jonosijaa", response = MuokattuJonosijaDTO.class)
    public Response poistaMuokattuJonosija(
            @ApiParam(value = "Valintatapajonon OID", required = true) @PathParam("valintatapajonoOid") String valintatapajonoOid,
            @ApiParam(value = "Hakemus OID", required = true) @PathParam("hakemusOid") String hakemusOid,
            @ApiParam(value = "Muokattavan järjestyskriteerin prioriteetti", required = true) @PathParam("jarjestyskriteeriPrioriteetti") Integer jarjestyskriteeriPrioriteetti,
            HttpServletRequest request) {
        User user = auditLog.getUser(request);

        MuokattuJonosija muokattuJonosija = tulosService.poistaMuokattuJonosija(valintatapajonoOid, hakemusOid, jarjestyskriteeriPrioriteetti, user);
        if (muokattuJonosija != null) {
            MuokattuJonosijaDTO map = modelMapper.map(muokattuJonosija, MuokattuJonosijaDTO.class);
            return Response.status(Response.Status.ACCEPTED).entity(map).build();
        } else {
            return Response.status(Response.Status.NOT_FOUND).build();
        }
    }

    @PUT
    @Consumes(MediaType.APPLICATION_JSON)
    @Produces(MediaType.APPLICATION_JSON)
    @Path("{valintatapajonoOid}/valmissijoiteltavaksi")
    @ApiOperation(value = "Tallentaa/muokkaa valintatapajonoa", response = ValintatapajonoDTO.class)
    public Response muokkaaSijotteluStatusta(@ApiParam(value = "Valintatapajonon OID", required = true) @PathParam("valintatapajonoOid") String valintatapajonoOid,
                                             @ApiParam(value = "Sijoittelustatus", required = true) @QueryParam("status") boolean status,
                                             @ApiParam(value = "Valintatapajono", required = true) ValintatapajonoDTO valintatapajono,
                                             HttpServletRequest request) {
        User user = auditLog.getUser(request);

        Consumer<Valintatapajono> valintatapajonoMuokkausFunktio = jono -> {
            // Käyttöliittymä kutsuu ValintaperusteetResourceV2::updateAutomaattinenSijoitteluunSiirto(valintatapajonoOid, status, request)
            jono.setAloituspaikat(valintatapajono.getAloituspaikat());
            jono.setEiVarasijatayttoa(valintatapajono.getEiVarasijatayttoa());
            jono.setKaikkiEhdonTayttavatHyvaksytaan(valintatapajono.getKaikkiEhdonTayttavatHyvaksytaan());
            jono.setKaytetaanValintalaskentaa(valintatapajono.getKaytetaanValintalaskentaa());
            jono.setNimi(valintatapajono.getNimi());
            jono.setPoissaOlevaTaytto(valintatapajono.getPoissaOlevaTaytto());
            jono.setPrioriteetti(valintatapajono.getPrioriteetti());
            jono.setSiirretaanSijoitteluun(valintatapajono.getSiirretaanSijoitteluun());
            jono.setValmisSijoiteltavaksi(status);
        };
        Optional<Valintatapajono> dto = tulosService.muokkaaValintatapajonoa(valintatapajonoOid, valintatapajonoMuokkausFunktio, user);
        return dto.map(jono -> Response.status(Response.Status.ACCEPTED).entity(modelMapper.map(jono, ValintatapajonoDTO.class)).build()).orElse(Response.status(Response.Status.NOT_FOUND).build());
    }

    @GET
    @Path("/{valintatapajonoOid}/valmissijoiteltavaksi")
    @Produces(MediaType.APPLICATION_JSON)
    public Response haeSijoitteluStatus(@ApiParam(value = "Valintatapajonon OID", required = true) @PathParam("valintatapajonoOid") String oid) {
        HashMap object = new HashMap();
        object.put("value", tulosService.haeSijoitteluStatus(oid));
        return Response.status(Response.Status.ACCEPTED).entity(object).build();
    }
}
