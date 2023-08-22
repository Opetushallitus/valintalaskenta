package fi.vm.sade.valintalaskenta.tulos;

import fi.vm.sade.auditlog.ApplicationType;
import fi.vm.sade.auditlog.Audit;
import fi.vm.sade.valinta.sharedutils.AuditLogger;

/**
 * @author Jussi Jartamo
 */
public class LaskentaAudit {
  public static final Audit AUDIT =
      new Audit(new AuditLogger(), "valintalaskenta-laskenta-service", ApplicationType.VIRKAILIJA);
}
