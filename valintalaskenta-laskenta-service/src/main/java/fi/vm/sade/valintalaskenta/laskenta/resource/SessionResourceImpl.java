package fi.vm.sade.valintalaskenta.laskenta.resource;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import javax.servlet.http.HttpServletRequest;
import org.springframework.http.MediaType;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;

@Controller
@Tag(name = "/session", description = "Sessionhallinta")
public class SessionResourceImpl {

  @PreAuthorize("isAuthenticated()")
  @Operation(
      summary = "Palauttaa session erääntymisen aikarajan sekunteina",
      description = "Tarvitsee HTTP kutsun, jossa on session id")
  @GetMapping(value = "/maxinactiveinterval", produces = MediaType.TEXT_PLAIN_VALUE)
  public String maxInactiveInterval(final HttpServletRequest req) {
    return Integer.toString(req.getSession().getMaxInactiveInterval());
  }
}
