package fi.vm.sade.valintalaskenta.laskenta.resource;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import javax.servlet.http.HttpServletRequest;
import org.springframework.http.MediaType;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@Tag(name = "/resources/session", description = "Sessionhallinta")
@RequestMapping(value = "/resources/session")
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
