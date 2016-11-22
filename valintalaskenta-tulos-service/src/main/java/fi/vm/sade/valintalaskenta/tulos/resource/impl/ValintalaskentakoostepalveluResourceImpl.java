package fi.vm.sade.valintalaskenta.tulos.resource.impl;

import fi.vm.sade.valintalaskenta.domain.dto.JonoDto;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import fi.vm.sade.valintalaskenta.domain.dto.ValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.ValintakoeOsallistuminenDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.HakemusOsallistuminenDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.ValintatietoValinnanvaiheDTO;
import fi.vm.sade.valintalaskenta.tulos.mapping.ValintalaskentaModelMapper;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import fi.vm.sade.valintalaskenta.tulos.service.impl.ValintatietoService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Component;
import org.springframework.stereotype.Controller;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.READ_UPDATE_CRUD;

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
            @PathParam("hakukohdeoid") String hakukohdeoid
    ) {
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
            @ApiParam(value = "Muokattava valinnanvaihe", required = true) ValinnanvaiheDTO vaihe) {
        try {
            return tulosService.lisaaTuloksia(vaihe, hakukohdeoid, tarjoajaOid);
        } catch (Exception e) {
            LOGGER.error("Valintatapajonon pisteitä ei saatu päivitettyä hakukohteelle " + hakukohdeoid, e);
            throw e;
        }
    }
}

