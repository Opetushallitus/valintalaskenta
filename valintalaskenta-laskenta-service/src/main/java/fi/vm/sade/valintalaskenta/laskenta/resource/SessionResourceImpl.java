package fi.vm.sade.valintalaskenta.laskenta.resource;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import fi.vm.sade.valintalaskenta.domain.resource.SessionResource;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;

@Controller
@Api(value = "/session", description = "Sessionhallinta")
public class SessionResourceImpl implements SessionResource {

    @GET
    @Path("/maxinactiveinterval")
    @PreAuthorize("isAuthenticated()")
    @Produces(MediaType.TEXT_PLAIN)
    @ApiOperation(
            value = "Palauttaa session erääntymisen aikarajan sekunteina",
            notes = "Tarvitsee HTTP kutsun, jossa on session id",
            response = String.class)
    public String maxInactiveInterval(@Context HttpServletRequest req) {
        return Integer.toString(req.getSession().getMaxInactiveInterval());
    }
}
