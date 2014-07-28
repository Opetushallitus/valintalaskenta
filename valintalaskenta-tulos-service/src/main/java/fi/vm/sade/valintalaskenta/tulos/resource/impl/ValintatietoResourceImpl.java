package fi.vm.sade.valintalaskenta.tulos.resource.impl;


import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.HakemusOsallistuminenDTO;
import fi.vm.sade.valintalaskenta.domain.dto.valintatieto.HakuDTO;
import fi.vm.sade.valintalaskenta.tulos.resource.ValintatietoResource;
import fi.vm.sade.valintalaskenta.tulos.service.impl.ValintatietoService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.ws.rs.*;
import java.util.List;

/**
 * Created by jukais on 25.3.2014.
 */
@Component
@Path("valintatieto")
public class ValintatietoResourceImpl implements ValintatietoResource {

    @Autowired
    private ValintatietoService valintatietoService;

    @Override
    @POST
    @Path("hakukohde/{hakukohdeOid}")
    @Consumes("application/json")
    @Produces("application/json")
    public List<HakemusOsallistuminenDTO> haeValintatiedotHakukohteelle(@PathParam("hakukohdeOid")String hakukohdeOid,
                                                                        List<String> valintakoeOid) {
        return valintatietoService.haeValintatiedotHakukohteelle(valintakoeOid,hakukohdeOid);
    }

    @Override
    @GET
    @Path("haku/{hakuOid}")
    @Produces("application/json")
    public HakuDTO haeValintatiedot(@PathParam("hakuOid") String hakuOid) {
        return valintatietoService.haeValintatiedot(hakuOid);
    }
}
