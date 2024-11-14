package fi.vm.sade.valintalaskenta.laskenta.resource.filter;

import jakarta.ws.rs.container.ContainerRequestContext;
import jakarta.ws.rs.container.ContainerResponseContext;
import jakarta.ws.rs.container.ContainerResponseFilter;
import jakarta.ws.rs.core.MultivaluedMap;
import java.io.IOException;

public class CorsResponseFilter implements ContainerResponseFilter {
  @Override
  public void filter(
      ContainerRequestContext containerRequestContext, ContainerResponseContext responseContext)
      throws IOException {
    MultivaluedMap<String, Object> headers = responseContext.getHeaders();
    headers.add("Access-Control-Allow-Origin", "*");
  }
}
