package fi.vm.sade.valintalaskenta.tulos.resource;

import fi.vm.sade.valintalaskenta.domain.JsonViews;
import fi.vm.sade.valintalaskenta.domain.valintakoe.ValintakoeOsallistuminen;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import org.codehaus.jackson.map.annotate.JsonView;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import java.util.List;

@Component
@Path("/valintakoe")
public class ValintakoeResource {

    @Autowired
    private ValintalaskentaTulosService tulosService;

    @GET
    @Produces(MediaType.APPLICATION_JSON)
    @Path("/hakija")
    @JsonView({ JsonViews.Basic.class })
    public List<ValintakoeOsallistuminen> kaikki() {
        return tulosService.haeValintakoeOsallistumiset();
    }

    @GET
    @Produces(MediaType.APPLICATION_JSON)
    @Path("/hakija/{hakijaOid}")
    @JsonView({ JsonViews.Basic.class })
    public List<ValintakoeOsallistuminen> haku(@PathParam("hakijaOid") String hakijaOid) {
        return tulosService.haeValintakoeOsallistumiset(hakijaOid);
    }

    @GET
    @Produces(MediaType.APPLICATION_JSON)
    @Path("/hakutoive/{hakukohdeOid}")
    @JsonView({ JsonViews.Basic.class })
    public List<ValintakoeOsallistuminen> hakuByHakutoive(@PathParam("hakukohdeOid") String hakukohdeOid) {
        return tulosService.haeValintakoeOsallistumisetByHakutoive(hakukohdeOid);
    }

}