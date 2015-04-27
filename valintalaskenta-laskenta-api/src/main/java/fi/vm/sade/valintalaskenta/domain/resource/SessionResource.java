package fi.vm.sade.valintalaskenta.domain.resource;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;

@Path("session")
public interface SessionResource {

    @GET
    @Path("/maxinactiveinterval")
    @Produces(MediaType.TEXT_PLAIN)
    public String maxInactiveInterval(@Context HttpServletRequest req);

}