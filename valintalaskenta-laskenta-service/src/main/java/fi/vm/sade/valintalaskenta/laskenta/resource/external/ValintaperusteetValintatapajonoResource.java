package fi.vm.sade.valintalaskenta.laskenta.resource.external;

import fi.vm.sade.service.valintaperusteet.dto.*;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.List;
import java.util.Map;

/**
 * Created with IntelliJ IDEA. User: jukais Date: 17.1.2013 Time: 14.42 To
 * change this template use File | Settings | File Templates.
 */
@Path("/valintaperusteet-service/resources/valintatapajono")
public interface ValintaperusteetValintatapajonoResource {

    @GET
    @Produces(MediaType.APPLICATION_JSON)
    @Path("/kopiot")
    Map<String, List<String>> findKopiot(@QueryParam("oid") List<String> oid);

    @GET
    @Path("/{oid}")
    @Produces(MediaType.APPLICATION_JSON)
    ValintatapajonoDTO readByOid(@PathParam("oid") String oid);

    @GET
    @Produces(MediaType.APPLICATION_JSON)
    @Path("/{oid}/jarjestyskriteeri")
    List<JarjestyskriteeriDTO> findJarjestyskriteeri(@PathParam("oid") String oid);

    @POST
    @Consumes(MediaType.APPLICATION_JSON)
    @Produces(MediaType.APPLICATION_JSON)
    @Path("/{valintatapajonoOid}/hakijaryhma/{hakijaryhmaOid}")
    Response liitaHakijaryhma(@PathParam("valintatapajonoOid") String valintatapajonoOid,
                              @PathParam("hakijaryhmaOid") String hakijaryhmaOid);

    @GET
    @Produces(MediaType.APPLICATION_JSON)
    @Path("/{valintatapajonoOid}/hakijaryhma")
    List<HakijaryhmaValintatapajonoDTO> hakijaryhmat(@PathParam("valintatapajonoOid") String valintatapajonoOid);

    @PUT
    @Consumes(MediaType.APPLICATION_JSON)
    @Produces(MediaType.APPLICATION_JSON)
    @Path("/{valintatapajonoOid}/hakijaryhma")
    Response insertHakijaryhma(@PathParam("valintatapajonoOid") String valintatapajonoOid, HakijaryhmaCreateDTO hakijaryhma);

    @GET
    @Produces(MediaType.APPLICATION_JSON)
    List<ValintatapajonoDTO> findAll();

    @POST
    @Path("/{oid}")
    @Consumes(MediaType.APPLICATION_JSON)
    @Produces(MediaType.APPLICATION_JSON)
    Response update(@PathParam("oid") String oid, ValintatapajonoCreateDTO jono);

    @PUT
    @Consumes(MediaType.APPLICATION_JSON)
    @Produces(MediaType.APPLICATION_JSON)
    @Path("/{valintatapajonoOid}/jarjestyskriteeri")
    Response insertJarjestyskriteeri(@PathParam("valintatapajonoOid") String valintatapajonoOid,
                                     JarjestyskriteeriInsertDTO jk) throws Exception;

    @POST
    @Consumes(MediaType.APPLICATION_JSON)
    @Produces(MediaType.APPLICATION_JSON)
    @Path("/jarjesta")
    List<ValintatapajonoDTO> jarjesta(List<String> oids);

    @DELETE
    @Path("/{oid}")
    Response delete(@PathParam("oid") String oid);


}
