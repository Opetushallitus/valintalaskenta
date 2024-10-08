package fi.vm.sade.valinta.kooste;

import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;

/** TODO maybe move to java-cas or some other suitable place on a sunnier day. */
public class AuthorizationUtil {

  public static String getCurrentUser() {
    if (getSecurityContext() == null || getAuthentication() == null) {
      return null;
    }
    return getAuthentication().getName();
  }

  private static Authentication getAuthentication() {
    return getSecurityContext().getAuthentication();
  }

  private static SecurityContext getSecurityContext() {
    return SecurityContextHolder.getContext();
  }
}
