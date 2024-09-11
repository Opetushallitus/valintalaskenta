package fi.vm.sade.valinta.kooste;

import fi.vm.sade.auditlog.ApplicationType;
import fi.vm.sade.auditlog.Audit;
import fi.vm.sade.valinta.sharedutils.AuditLogger;
import java.security.Principal;
import java.util.Optional;
import org.springframework.security.cas.authentication.CasAuthenticationToken;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;

/**
 * @author Jussi Jartamo
 */
public class KoosteAudit {
  public static final Audit AUDIT =
      new Audit(new AuditLogger(), "valintalaskentakoostepalvelu", ApplicationType.VIRKAILIJA);

  public static Optional<String> uid() {
    Optional<String> uid = uidFromCasAuthenticationToken();
    if (!uid.isPresent()) {
      uid = uidFromUserDetails();
    }
    return uid;
  }

  private static Optional<String> uidFromCasAuthenticationToken() {
    if (SecurityContextHolder.getContext().getAuthentication() instanceof CasAuthenticationToken) {
      CasAuthenticationToken casAuthenticationToken =
          (CasAuthenticationToken) SecurityContextHolder.getContext().getAuthentication();
      if (null != casAuthenticationToken.getAssertion()
          && null != casAuthenticationToken.getAssertion().getPrincipal()) {
        return Optional.ofNullable(casAuthenticationToken.getAssertion().getPrincipal().getName());
      }
    }
    return Optional.empty();
  }

  private static Optional<String> uidFromUserDetails() {
    SecurityContext context = SecurityContextHolder.getContext();
    if (context != null) {
      Principal p = context.getAuthentication();
      if (p != null && p.getName() != null) {
        return Optional.of(p.getName());
      }
    }

    return Optional.empty();
  }
}
