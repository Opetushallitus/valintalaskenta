package fi.vm.sade.valintalaskenta.tulos.resource;

import com.wordnik.swagger.annotations.Api;
import com.wordnik.swagger.annotations.ApiOperation;
import com.wordnik.swagger.annotations.ApiParam;
import fi.vm.sade.valintalaskenta.domain.JsonViews;
import fi.vm.sade.valintalaskenta.domain.dto.valintakoe.ValintakoeOsallistuminenDTO;
import fi.vm.sade.valintalaskenta.tulos.mapping.ValintalaskentaModelMapper;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import org.codehaus.jackson.map.annotate.JsonView;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.annotation.Secured;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Component;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import java.util.List;

import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.*;

@Component
@Path("valintakoe")
@PreAuthorize("isAuthenticated()")
@Api(value = "/valintakoe", description = "Resurssi valintakoeosallistumistulosten hakemiseen")
public class ValintakoeResource {

    @Autowired
    private ValintalaskentaTulosService tulosService;

    @Autowired
    private ValintalaskentaModelMapper modelMapper;

    @GET
    @Produces(MediaType.APPLICATION_JSON)
    @Path("hakemus/{hakemusOid}")
    @JsonView({JsonViews.Basic.class})
    @Secured({READ, UPDATE, CRUD})
    @ApiOperation(value = "Hakee valintakoeosallistumiset hakemukselle OID:n perusteella", response = ValintakoeOsallistuminenDTO.class)
    public List<ValintakoeOsallistuminenDTO> haku(@ApiParam(value = "Hakemus OID", required = true) @PathParam("hakemusOid") String hakemusOid) {
        return modelMapper.mapList(tulosService.haeValintakoeOsallistumiset(hakemusOid), ValintakoeOsallistuminenDTO.class);
    }

    @GET
    @Produces(MediaType.APPLICATION_JSON)
    @Path("hakutoive/{hakukohdeOid}")
    @JsonView({JsonViews.Basic.class})
    @Secured({READ, UPDATE, CRUD})
    @ApiOperation(value = "Hakee valintakoeosallistumiset hakukohteelle OID:n perusteella", response = ValintakoeOsallistuminenDTO.class)
    public List<ValintakoeOsallistuminenDTO> hakuByHakutoive(@ApiParam(value = "Hakukohde OID", required = true) @PathParam("hakukohdeOid") String hakukohdeOid) {
        return modelMapper.mapList(tulosService.haeValintakoeOsallistumisetByHakutoive(hakukohdeOid), ValintakoeOsallistuminenDTO.class);
    }

}
