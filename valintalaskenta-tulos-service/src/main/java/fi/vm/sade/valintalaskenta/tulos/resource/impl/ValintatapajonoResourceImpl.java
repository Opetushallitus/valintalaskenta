package fi.vm.sade.valintalaskenta.tulos.resource.impl;

import com.wordnik.swagger.annotations.Api;
import com.wordnik.swagger.annotations.ApiOperation;
import com.wordnik.swagger.annotations.ApiParam;
import fi.vm.sade.service.valintaperusteet.resource.ValintaperusteetResource;
import fi.vm.sade.valintalaskenta.domain.dto.MuokattuJonosijaArvoDTO;
import fi.vm.sade.valintalaskenta.domain.dto.MuokattuJonosijaDTO;
import fi.vm.sade.valintalaskenta.domain.dto.ValintatapajonoDTO;
import fi.vm.sade.valintalaskenta.domain.valinta.MuokattuJonosija;
import fi.vm.sade.valintalaskenta.domain.valinta.Valintatapajono;
import fi.vm.sade.valintalaskenta.tulos.mapping.ValintalaskentaModelMapper;
import fi.vm.sade.valintalaskenta.tulos.resource.ValintatapajonoResource;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Component;
import org.springframework.stereotype.Controller;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.HashMap;
import java.util.Optional;

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
    private ValintaperusteetResource valintaperusteetResource;

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
            @ApiParam(value = "Järjestyskriteerin uusi arvo", required = true) MuokattuJonosijaArvoDTO arvo) {

        MuokattuJonosija muokattuJonosija = tulosService.muutaJarjestyskriteeri(valintatapajonoOid, hakemusOid, jarjestyskriteeriPrioriteetti, arvo);
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
            @ApiParam(value = "Muokattavan järjestyskriteerin prioriteetti", required = true) @PathParam("jarjestyskriteeriPrioriteetti") Integer jarjestyskriteeriPrioriteetti) {
        MuokattuJonosija muokattuJonosija = tulosService.poistaMuokattuJonosija(valintatapajonoOid, hakemusOid, jarjestyskriteeriPrioriteetti);
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
    @ApiOperation(value = "Lisää/Poistaa valintatapajonon sijoittelusta", response = ValintatapajonoDTO.class)
    public Response muokkaaSijotteluStatusta(@ApiParam(value = "Valintatapajonon OID", required = true) @PathParam("valintatapajonoOid") String valintatapajonoOid,
                                             @ApiParam(value = "Sijoittelustatus", required = true) @QueryParam("status") boolean status) {
        Optional<Valintatapajono> dto = tulosService.muokkaaValintatapajonoa(valintatapajonoOid,
                jono -> {
                    fi.vm.sade.service.valintaperusteet.dto.ValintatapajonoDTO jonoDto = valintaperusteetResource.updateAutomaattinenSijoitteluunSiirto(valintatapajonoOid, status);
                    jono.setAloituspaikat(jonoDto.getAloituspaikat());
                    jono.setEiVarasijatayttoa(jonoDto.getEiVarasijatayttoa());
                    //jono.setJonosijat(jonoDto.get);
                    jono.setKaikkiEhdonTayttavatHyvaksytaan(jonoDto.getKaikkiEhdonTayttavatHyvaksytaan());
                    jono.setKaytetaanValintalaskentaa(jonoDto.getKaytetaanValintalaskentaa());
                    jono.setNimi(jonoDto.getNimi());
                    jono.setPoissaOlevaTaytto(jonoDto.getPoissaOlevaTaytto());
                    jono.setPrioriteetti(jonoDto.getPrioriteetti());
                    jono.setSiirretaanSijoitteluun(jonoDto.getSiirretaanSijoitteluun());
                    //jono.setSijoitteluajoId(jonoDto.getS);
                    //jono.setTasasijasaanto(jonoDto.get);
                    //jono.setValintatapajonoOid();
                    jono.setValmisSijoiteltavaksi(status);
                });
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
