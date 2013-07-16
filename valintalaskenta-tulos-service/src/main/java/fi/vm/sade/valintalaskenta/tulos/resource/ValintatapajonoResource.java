package fi.vm.sade.valintalaskenta.tulos.resource;

import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.CRUD;
import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.UPDATE;

import java.math.BigDecimal;

import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.annotation.Secured;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Component;

import fi.vm.sade.valintalaskenta.domain.Valintatapajono;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;

/**
 * @author Jussi Jartamo
 */
@Component
@Path("valintatapajono")
@PreAuthorize("isAuthenticated()")
public class ValintatapajonoResource {

    @Autowired
    private ValintalaskentaTulosService tulosService;

    /*
     * @GET
     * 
     * @Path("{valintatapajonoid}/jarjestyskriteeritulos")
     * 
     * @Produces(MediaType.APPLICATION_JSON)
     * 
     * @JsonView({ JsonViews.Basic.class }) public List<Jonosija>
     * valinnanvaihe(@PathParam("valintatapajonoid") String valintatapajonoid) {
     * // kaikki versiot tästä valintatapajonosta return
     * tulosService.haeJarjestyskriteerituloksetValintatapajonolle
     * (valintatapajonoid); }
     */

    @POST
    @Consumes(MediaType.APPLICATION_JSON)
    @Produces(MediaType.APPLICATION_JSON)
    @Path("{valintatapajonoOid}/{hakemusOid}/{jarjestyskriteeriPrioriteetti}/arvo")
    @Secured({ UPDATE, CRUD })
    public Valintatapajono muutaJarjestyskriteerinArvo(@PathParam("valintatapajonoOid") String valintatapajonoOid,
            @PathParam("hakemusOid") String hakemusOid,
            @PathParam("jarjestyskriteeriPrioriteetti") Integer jarjestyskriteeriPrioriteetti, BigDecimal arvo) {
        return tulosService.muutaJarjestyskriteerinArvo(valintatapajonoOid, hakemusOid, jarjestyskriteeriPrioriteetti,
                arvo);
    }

}
