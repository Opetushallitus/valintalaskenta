package fi.vm.sade.valintalaskenta.tulos.resource;

import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.CRUD;
import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.READ;
import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.UPDATE;

import java.util.List;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import org.codehaus.jackson.map.annotate.JsonView;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.annotation.Secured;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Component;

import fi.vm.sade.valintalaskenta.domain.JsonViews;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;

@Component
@Path("valintakoe")
@PreAuthorize("isAuthenticated()")
public class ValintakoeResource {

    @Autowired
    private ValintalaskentaTulosService tulosService;

    @GET
    @Produces(MediaType.APPLICATION_JSON)
    @Path("hakija")
    @JsonView({ JsonViews.Basic.class })
    @Secured({ READ, UPDATE, CRUD })
    public List<ValintakoeOsallistuminen> kaikki() {
        return tulosService.haeValintakoeOsallistumiset();
    }

    @GET
    @Produces(MediaType.APPLICATION_JSON)
    @Path("hakija/{hakijaOid}")
    @JsonView({ JsonViews.Basic.class })
    @Secured({ READ, UPDATE, CRUD })
    public List<ValintakoeOsallistuminen> haku(@PathParam("hakijaOid") String hakijaOid) {
        return tulosService.haeValintakoeOsallistumiset(hakijaOid);
    }

    @GET
    @Produces(MediaType.APPLICATION_JSON)
    @Path("hakutoive/{hakukohdeOid}")
    @JsonView({ JsonViews.Basic.class })
    @Secured({ READ, UPDATE, CRUD })
    public List<ValintakoeOsallistuminen> hakuByHakutoive(@PathParam("hakukohdeOid") String hakukohdeOid) {
        return tulosService.haeValintakoeOsallistumisetByHakutoive(hakukohdeOid);
    }

}
