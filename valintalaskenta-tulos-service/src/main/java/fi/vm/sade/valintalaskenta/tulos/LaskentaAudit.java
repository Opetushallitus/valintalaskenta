package fi.vm.sade.valintalaskenta.tulos;

import fi.vm.sade.auditlog.ApplicationType;
import fi.vm.sade.auditlog.Audit;
import fi.vm.sade.sharedutils.AuditLogger;
import org.springframework.security.core.context.SecurityContextHolder;

import java.security.Principal;
import java.util.Optional;

/**
 * @author Jussi Jartamo
 */
public class LaskentaAudit {
    public static final Audit AUDIT = new Audit(new AuditLogger(), "valintalaskenta-laskenta-service", ApplicationType.VIRKAILIJA);

    public static String username() {
        return Optional.ofNullable((Principal) SecurityContextHolder.getContext().getAuthentication()).orElse(
                () -> "Kirjautumaton käyttäjä"
        ).getName();
    }
}
