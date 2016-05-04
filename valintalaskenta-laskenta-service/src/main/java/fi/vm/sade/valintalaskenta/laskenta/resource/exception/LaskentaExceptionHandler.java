package fi.vm.sade.valintalaskenta.laskenta.resource.exception;

import fi.vm.sade.valintalaskenta.laskenta.service.exception.LaskentaEdellinenValinnanvaiheLaskemattaException;

import javax.ws.rs.core.Response;
import javax.ws.rs.ext.ExceptionMapper;
import javax.ws.rs.ext.Provider;

@Provider
public class LaskentaExceptionHandler implements ExceptionMapper<LaskentaEdellinenValinnanvaiheLaskemattaException> {
    public Response toResponse(LaskentaEdellinenValinnanvaiheLaskemattaException exception) {
        Response.Status status = Response.Status.INTERNAL_SERVER_ERROR;

        return Response.status(status).entity(exception.getMessage()).build();
    }
}