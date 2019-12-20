package fi.vm.sade.valintalaskenta.tulos.resource.impl;

import fi.vm.sade.auditlog.User;
import fi.vm.sade.valintalaskenta.domain.dto.JonoDto;
import fi.vm.sade.valintalaskenta.domain.dto.ValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.ValintakoeOsallistuminenDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.HakemusOsallistuminenDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.ValintatietoValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.tulos.logging.LaskentaAuditLog;
import fi.vm.sade.valintalaskenta.tulos.mapping.ValintalaskentaModelMapper;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import fi.vm.sade.valintalaskenta.tulos.service.impl.ValintatietoService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import java.util.List;

@Controller
@Path("valintalaskentakoostepalvelu")
@Api(value = "/valintalaskentakoostepalvelu", description = "Resurssi tulosten hakemiseen hakukohteittain")
public class ValintalaskentakoostepalveluResourceImpl {
    protected static final Logger LOGGER = LoggerFactory.getLogger(ValintalaskentakoostepalveluResourceImpl.class);

    @Autowired
    private ValintalaskentaTulosService tulosService;

    @Autowired
    private ValintalaskentaModelMapper modelMapper;
    @Autowired
    private ValintatietoService valintatietoService;

    @Autowired
    private LaskentaAuditLog auditLogger;

    /**
     * @param hakuOid
     * @return HAKUKOHDE OID -> LIST[VALINTATAPAJONO OID]
     */
    @GET
    @Path("jonotsijoittelussa/{hakuOid}")
    @Consumes("application/json")
    @Produces("application/json")
    public List<JonoDto> jonotSijoittelussa(@PathParam("hakuOid") String hakuOid) {
        return tulosService.haeJonotSijoittelussa(hakuOid);
    }

    @POST
    @Path("valintatieto/hakukohde/{hakukohdeOid}")
    @Consumes("application/json")
    @Produces("application/json")
    public List<HakemusOsallistuminenDTO> haeValintatiedotHakukohteelle(@PathParam("hakukohdeOid") String hakukohdeOid,
                                                                        List<String> valintakoeTunnisteet) {
        return valintatietoService.haeValintatiedotHakukohteelle(valintakoeTunnisteet, hakukohdeOid);
    }

    @GET
    @Produces(MediaType.APPLICATION_JSON)
    @Path("valintakoe/hakutoive/{hakukohdeOid}")
    @ApiOperation(value = "Hakee valintakoeosallistumiset hakukohteelle OID:n perusteella", response = ValintakoeOsallistuminenDTO.class)
    public List<ValintakoeOsallistuminenDTO> hakuByHakutoive(
            @ApiParam(value = "Hakukohde OID", required = true) @PathParam("hakukohdeOid") String hakukohdeOid) {
        return modelMapper.mapList(tulosService.haeValintakoeOsallistumisetByHakutoive(hakukohdeOid), ValintakoeOsallistuminenDTO.class);
    }

    @POST
    @Consumes(MediaType.APPLICATION_JSON)
    @Produces(MediaType.APPLICATION_JSON)
    @Path("valintakoe/hakutoive/")
    @ApiOperation(value = "Hakee valintakoeosallistumiset hakukohteelle OID:n perusteella", response = ValintakoeOsallistuminenDTO.class)
    public List<ValintakoeOsallistuminenDTO> hakuByOids(
            List<String> hakukohdeOids) {
        return modelMapper.mapList(tulosService.haeValintakoeOsallistumisetByHakukohdes(hakukohdeOids), ValintakoeOsallistuminenDTO.class);
    }

    @GET
    @Path("/hakukohde/{hakukohdeoid}/valinnanvaihe")
    @Produces(MediaType.APPLICATION_JSON)
    @ApiOperation(value = "Hakee hakukohteen valinnan vaiheiden tulokset", response = ValinnanvaiheDTO.class)
    public List<ValintatietoValinnanvaiheDTO> hakukohde(
            @ApiParam(value = "Hakukohteen OID", required = true)
            @PathParam("hakukohdeoid") String hakukohdeoid) {
        return tulosService.haeValinnanvaiheetHakukohteelle(hakukohdeoid);
    }

    @POST
    @Path("/hakukohde/{hakukohdeoid}/valinnanvaihe")
    @Produces(MediaType.APPLICATION_JSON)
    @Consumes(MediaType.APPLICATION_JSON)
    @ApiOperation(value = "Lisää tuloksia valinnanvaiheelle", response = ValinnanvaiheDTO.class)
    public ValinnanvaiheDTO lisaaTuloksia(
            @ApiParam(value = "Hakukohteen OID", required = true) @PathParam("hakukohdeoid") String hakukohdeoid,
            @ApiParam(value = "Tarjoaja OID", required = true) @QueryParam("tarjoajaOid") String tarjoajaOid,
            @ApiParam(value = "Muokattava valinnanvaihe", required = true) ValinnanvaiheDTO vaihe,
            HttpServletRequest request) {
        try {
            User user = auditLogger.getUser(request);

            return tulosService.lisaaTuloksia(vaihe, hakukohdeoid, tarjoajaOid);
        } catch (Exception e) {
            LOGGER.error("Valintatapajonon pisteitä ei saatu päivitettyä hakukohteelle " + hakukohdeoid, e);
            throw e;
        }
    }

    @GET
    @Path("valintakoe/hakemus/{hakemusOid}")
    @Produces(MediaType.APPLICATION_JSON)
    @ApiOperation(value = "Hakee hakemuksen valintakoeosallistumistiedot", response = ValintakoeOsallistuminenDTO.class)
    public ValintakoeOsallistuminenDTO haeHakemuksenValintakoeosallistumistiedot(
        @ApiParam(value = "Hakemus OID", required = true) @PathParam("hakemusOid") String hakemusOid) {
        return modelMapper.map(tulosService.haeValintakoeOsallistumiset(hakemusOid), ValintakoeOsallistuminenDTO.class);
    }
}

