package fi.vm.sade.valintalaskenta.tulos.resource;

import fi.vm.sade.valintalaskenta.domain.JsonViews;
import fi.vm.sade.valintalaskenta.domain.valinta.HarkinnanvarainenHyvaksyminen;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import org.codehaus.jackson.map.annotate.JsonView;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.annotation.Secured;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Component;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;

import java.util.List;

import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.*;

/**
 *
 * @author Jussi Jartamo
 *
 */
@Component
@Path("harkinnanvarainenhyvaksynta")
@PreAuthorize("isAuthenticated()")
public class HarkinnanvaraisuusResource {

    @Autowired
    private ValintalaskentaTulosService tulosService;

    @POST
    @Path("/haku/{hakuOid}/hakukohde/{hakukohdeOid}/hakemus/{hakemusOid}")
    @Produces(MediaType.APPLICATION_JSON)
    @JsonView({ JsonViews.Basic.class })
    @Secured({ UPDATE, CRUD })
    public void hakemus(@PathParam("hakuOid") String hakuOid,
                        @PathParam("hakukohdeOid") String hakukohdeOid,
                        @PathParam("hakemusOid") String hakemusOid,
                        HarkinnanvarainenHyvaksyminen harkinnanvarainenHyvaksyminen) {
        tulosService.asetaHarkinnanvaraisestiHyvaksymisenTila(hakuOid,hakukohdeOid,hakemusOid,harkinnanvarainenHyvaksyminen.getHyvaksyttyHarkinnanvaraisesti());
    }

    @GET
    @Path("/haku/{hakuOid}/hakukohde/{hakukohdeOid}")
    @Produces(MediaType.APPLICATION_JSON)
    @JsonView({ JsonViews.Basic.class })
    @Secured({ READ, UPDATE, CRUD })
    public List<HarkinnanvarainenHyvaksyminen> hakemus(@PathParam("hakuOid") String hakuOid,
                                                       @PathParam("hakukohdeOid") String hakukohdeOid) {
        return  tulosService.haeHarkinnanvaraisestiHyvaksymisenTila(hakukohdeOid);
    }
}
