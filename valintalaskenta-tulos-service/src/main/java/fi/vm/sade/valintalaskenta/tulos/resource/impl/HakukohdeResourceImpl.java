package fi.vm.sade.valintalaskenta.tulos.resource.impl;

import static fi.vm.sade.auditlog.valintaperusteet.LogMessage.builder;
import static fi.vm.sade.valintalaskenta.tulos.LaskentaAudit.AUDIT;
import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.READ_UPDATE_CRUD;
import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.ROLE_VALINTOJENTOTEUTTAMINEN_TULOSTENTUONTI;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import fi.vm.sade.auditlog.valintaperusteet.ValintaperusteetOperation;
import fi.vm.sade.authentication.business.service.Authorizer;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.service.valintaperusteet.resource.ValintaperusteetResource;
import fi.vm.sade.valintalaskenta.domain.dto.HakijaryhmaDTO;
import fi.vm.sade.valintalaskenta.domain.dto.ValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.ValintatietoValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.tulos.resource.HakukohdeResource;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import fi.vm.sade.valintalaskenta.tulos.resource.HakukohdeResource;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.List;
import java.util.Optional;

import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.READ_UPDATE_CRUD;
import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.ROLE_VALINTOJENTOTEUTTAMINEN_TULOSTENTUONTI;

import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

@Controller
@Path("hakukohde")
@PreAuthorize("isAuthenticated()")
@Api(value = "/hakukohde", description = "Resurssi tulosten hakemiseen hakukohteittain")
public class HakukohdeResourceImpl implements HakukohdeResource {
    protected static final Logger LOGGER = LoggerFactory.getLogger(HakukohdeResourceImpl.class);

    @Autowired
    private ValintalaskentaTulosService tulosService;

    @Autowired
    private Authorizer authorizer;

    @Autowired
    private ValintaperusteetResource valintaperusteetResource;

    @GET
    @Path("{hakukohdeoid}/valinnanvaihe")
    @Produces(MediaType.APPLICATION_JSON)
    @PreAuthorize(READ_UPDATE_CRUD)
    @ApiOperation(value = "Hakee hakukohteen valinnan vaiheiden tulokset", response = ValinnanvaiheDTO.class)
    public List<ValintatietoValinnanvaiheDTO> hakukohde(
            @ApiParam(value = "Hakukohteen OID", required = true) @PathParam("hakukohdeoid") String hakukohdeoid) {
        try {
            return tulosService.haeValinnanvaiheetHakukohteelle(hakukohdeoid);
        } catch (Exception e) {
            LOGGER.error("Valintalaskennan tulosten haku hakukohteelle {} epäonnistui!", hakukohdeoid, e);
            throw e;
        }

    }

    @POST
    @Path("{hakukohdeoid}/valinnanvaihe")
    @Produces(MediaType.APPLICATION_JSON)
    @Consumes(MediaType.APPLICATION_JSON)
    @PreAuthorize(READ_UPDATE_CRUD)
    @ApiOperation(value = "Lisää tuloksia valinnanvaiheelle", response = ValinnanvaiheDTO.class)
    public Response lisaaTuloksia(
            @ApiParam(value = "Hakukohteen OID", required = true) @PathParam("hakukohdeoid") String hakukohdeoid,
            @ApiParam(value = "Tarjoaja OID", required = true) @QueryParam("tarjoajaOid") String tarjoajaOid,
            @ApiParam(value = "Muokattava valinnanvaihe", required = true) ValinnanvaiheDTO vaihe) {
        try {
            authorizer.checkOrganisationAccess(tarjoajaOid, ROLE_VALINTOJENTOTEUTTAMINEN_TULOSTENTUONTI);

            List<ValintaperusteetDTO> valintaperusteet = valintaperusteetResource.haeValintaperusteet(hakukohdeoid, null);
            if (vaihe.empty()) {
                Map<String,String> message = new HashMap<>();
                LOGGER.warn(String.format("Saatiin tyhjä data käyttöliittymältä; ei tallenneta. " +
                    "tarjoajaOid = %s, hakukohdeOid = %s , syöte: %s", tarjoajaOid, hakukohdeoid, vaihe));
                message.put("error", "Saatiin tyhjä data käyttöliittymältä; ei tallenneta.");
                return Response.status(Response.Status.BAD_REQUEST).entity(message).build();
            }

            Optional<ValintaperusteetDTO> valinnanVaiheValintaperusteissa = valintaperusteet.stream()
                    .filter(valintaperuste -> valintaperuste.getValinnanVaihe().getValinnanVaiheOid().equals(vaihe.getValinnanvaiheoid()))
                    .findAny();

            if (!valinnanVaiheValintaperusteissa.isPresent()) {
                LOGGER.error("Päivitettävää valinnanvaihetta ei löytynyt valintaperusteista, hakukohde {}, valinnanvaihe {}", hakukohdeoid, vaihe.getValinnanvaiheoid());
                return Response.status(Response.Status.INTERNAL_SERVER_ERROR).build();
            }
        } catch (Exception e) {
            LOGGER.error("Valintatapajonon pisteitä ei saatu päivitettyä hakukohteelle " + hakukohdeoid,e);
            return Response.status(Response.Status.INTERNAL_SERVER_ERROR).build();
        }
        ValinnanvaiheDTO valinnanvaihe = tulosService.lisaaTuloksia(vaihe, hakukohdeoid, tarjoajaOid);
        auditLog(hakukohdeoid, vaihe);
        return Response.status(Response.Status.ACCEPTED).entity(valinnanvaihe).build();
    }

    private void auditLog(@ApiParam(value = "Hakukohteen OID", required = true) @PathParam("hakukohdeoid") String hakukohdeoid, @ApiParam(value = "Muokattava valinnanvaihe", required = true) ValinnanvaiheDTO vaihe) {
        vaihe.getValintatapajonot()
                .forEach(
                        v -> {
                            v.getHakija().forEach(h -> {
                                //AuditLog.log(ValintaperusteetOperation.VALINNANVAIHE_TUONTI_KAYTTOLIITTYMA, ValintaResource);
                                /*
                                AUDIT.log(builder()
                                        .id(LaskentaAudit.username())
                                        .hakemusOid(h.getHakemusOid())
                                        .valinnanvaiheOid(vaihe.getValinnanvaiheoid())
                                        .hakukohdeOid(hakukohdeoid)
                                        .valintatapajonoOid(v.getOid())
                                        .add("jonosija", h.getJonosija())
                                        .setOperaatio(ValintaperusteetOperation.VALINNANVAIHE_TUONTI_KAYTTOLIITTYMA)
                                        .build());*/
                            });
                        }
                );
    }

    @GET
    @Path("{hakukohdeoid}/hakijaryhma")
    @Produces(MediaType.APPLICATION_JSON)
    @ApiOperation(value = "Hakee hakukohteen hakijaryhmien tulokset", response = HakijaryhmaDTO.class)
    public List<HakijaryhmaDTO> hakijaryhmat(@ApiParam(value = "Hakukohteen OID", required = true) @PathParam("hakukohdeoid") String hakukohdeoid) {
        try {
            return tulosService.haeHakijaryhmatHakukohteelle(hakukohdeoid);
        } catch (Exception e) {
            LOGGER.error("Hakijaryhmien tulosten haku hakukohteelle {} epäonnistui!", hakukohdeoid, e);
            throw e;
        }
    }
}
