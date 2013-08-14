package fi.vm.sade.valintalaskenta.tulos.resource;

import fi.vm.sade.valintalaskenta.domain.JarjestyskriteerituloksenTila;
import fi.vm.sade.valintalaskenta.domain.MuokattuJonosija;
import fi.vm.sade.valintalaskenta.tulos.service.ValintalaskentaTulosService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.annotation.Secured;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Component;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import java.math.BigDecimal;

import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.CRUD;
import static fi.vm.sade.valintalaskenta.tulos.roles.ValintojenToteuttaminenRole.UPDATE;

/**
 * @author Jussi Jartamo
 */
@Component
@Path("valintatapajono")
@PreAuthorize("isAuthenticated()")
public class ValintatapajonoResource {

    @Autowired
    private ValintalaskentaTulosService tulosService;


    /**
     * @param valintatapajonoOid
     * @param hakemusOid
     * @param jarjestyskriteeriPrioriteetti
     * @param arvo
     * @return
     */
    @POST
    @Consumes(MediaType.APPLICATION_JSON)
    @Produces(MediaType.APPLICATION_JSON)
    @Path("{valintatapajonoOid}/{hakemusOid}/{jarjestyskriteeriPrioriteetti}/arvo")
    @Secured({ UPDATE, CRUD })
    public MuokattuJonosija muutaJarjestyskriteerinArvo(
                                                        @PathParam("valintatapajonoOid") String valintatapajonoOid,
                                                        @PathParam("hakemusOid") String hakemusOid,
                                                        @PathParam("jarjestyskriteeriPrioriteetti") Integer jarjestyskriteeriPrioriteetti,
                                                        BigDecimal arvo,
                                                        @QueryParam("selite") String selite) {
        return tulosService.muutaJarjestyskriteerinArvo( valintatapajonoOid, hakemusOid, jarjestyskriteeriPrioriteetti, arvo, selite);
    }


    /**
     * @param valintatapajonoOid
     * @param hakemusOid
     * @param jarjestyskriteeriPrioriteetti
     * @param tila
     * @return
     */
    @POST
    @Consumes(MediaType.APPLICATION_JSON)
    @Produces(MediaType.APPLICATION_JSON)
    @Path("{valintatapajonoOid}/{hakemusOid}/{jarjestyskriteeriPrioriteetti}/tila")
    @Secured({ UPDATE, CRUD })
    public MuokattuJonosija muutaJarjestyskriteerinTila(
                                                        @PathParam("valintatapajonoOid") String valintatapajonoOid,
                                                        @PathParam("hakemusOid") String hakemusOid,
                                                        @PathParam("jarjestyskriteeriPrioriteetti") Integer jarjestyskriteeriPrioriteetti,
                                                        String tila,
                                                        @QueryParam("selite") String selite) {
        return tulosService.muutaJarjestyskriteerinTila( valintatapajonoOid, hakemusOid, jarjestyskriteeriPrioriteetti, JarjestyskriteerituloksenTila.valueOf(tila), selite);
    }
}
