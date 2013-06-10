package fi.vm.sade.valintalaskenta.tulos.resource;

import fi.vm.sade.valintalaskenta.domain.Valintatapajono;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.annotation.Secured;
import org.springframework.stereotype.Component;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;

import static fi.vm.sade.valintalaskenta.tulos.resource.roles.ValintojenToteuttaminenRole.CRUD;
import static fi.vm.sade.valintalaskenta.tulos.resource.roles.ValintojenToteuttaminenRole.READ;
import static fi.vm.sade.valintalaskenta.tulos.resource.roles.ValintojenToteuttaminenRole.UPDATE;

/**
 * @author Jussi Jartamo
 */
@Component
@Path("/valintatapajono")
public class ValintatapajonoResource {

    @Autowired
    private ValintalaskentaTulosService tulosService;
  /*
    @GET
    @Path("{valintatapajonoid}/jarjestyskriteeritulos")
    @Produces(MediaType.APPLICATION_JSON)
    @JsonView({ JsonViews.Basic.class })
    public List<Jonosija> valinnanvaihe(@PathParam("valintatapajonoid") String valintatapajonoid) {
        // kaikki versiot tästä valintatapajonosta
        return tulosService.haeJarjestyskriteerituloksetValintatapajonolle(valintatapajonoid);
    }
    */

    @POST
    @Consumes(MediaType.APPLICATION_JSON)
    @Produces(MediaType.APPLICATION_JSON)
    @Path("{valintatapajonoOid}/{hakemusOid}/{jarjestyskriteeriPrioriteetti}/arvo")
    @Secured({UPDATE, CRUD})
    public Valintatapajono muutaJarjestyskriteerinArvo(@PathParam("valintatapajonoOid") String valintatapajonoOid,
                                                       @PathParam("hakemusOid") String hakemusOid,
                                                       @PathParam("jarjestyskriteeriPrioriteetti") Integer jarjestyskriteeriPrioriteetti,
                                                       Double arvo) {
        return tulosService.muutaJarjestyskriteerinArvo(valintatapajonoOid, hakemusOid, jarjestyskriteeriPrioriteetti, arvo);
    }

}
