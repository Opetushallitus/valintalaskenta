package fi.vm.sade.valintalaskenta.tulos.service;

import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;

/** TODO maybe move to java-cas or some other suitable place on a sunnier day. */
public class AuthorizationUtil {
  public static String getCurrentUser() {
    SecurityContext ctx = SecurityContextHolder.getContext();
    if (ctx == null) {
      return null;
    }
    Authentication authentication = ctx.getAuthentication();
    if (authentication == null) {
      return null;
    }
    return authentication.getName();
  }
}
