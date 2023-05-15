package fi.vm.sade.valintalaskenta.laskenta.resource;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import javax.servlet.http.HttpServletRequest;

import org.springframework.http.MediaType;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;

@Controller
@Api(value = "/session", description = "Sessionhallinta")
public class SessionResourceImpl {

  @PreAuthorize("isAuthenticated()")
  @ApiOperation(
      value = "Palauttaa session erääntymisen aikarajan sekunteina",
      notes = "Tarvitsee HTTP kutsun, jossa on session id",
      response = String.class)
  @GetMapping(value = "/maxinactiveinterval", produces = MediaType.TEXT_PLAIN_VALUE)
  public String maxInactiveInterval(final HttpServletRequest req) {
    return Integer.toString(req.getSession().getMaxInactiveInterval());
  }
}
