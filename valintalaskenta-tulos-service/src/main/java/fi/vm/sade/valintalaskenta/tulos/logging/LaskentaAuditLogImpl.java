package fi.vm.sade.valintalaskenta.tulos.logging;

import fi.vm.sade.auditlog.Audit;
import fi.vm.sade.auditlog.Changes;
import fi.vm.sade.auditlog.Operation;
import fi.vm.sade.auditlog.User;
import fi.vm.sade.valinta.sharedutils.AuditLog;
import fi.vm.sade.valinta.sharedutils.ValintaResource;
import java.util.Map;
import javax.servlet.http.HttpServletRequest;

public class LaskentaAuditLogImpl implements LaskentaAuditLog {

  @Override
  public User getUser(HttpServletRequest request) {
    if (request == null) {
      String msg =
          "Cannot get user for auditlogging from null request! Is something wrong with the @Context?";
      throw new RuntimeException(msg);
    }
    return AuditLog.getUser(request);
  }

  @Override
  public <T> void log(
      Audit audit,
      User user,
      Operation operation,
      ValintaResource valintaResource,
      String targetOid,
      Changes changes,
      Map<String, String> additionalInfo) {
    AuditLog.log(audit, user, operation, valintaResource, targetOid, changes, additionalInfo);
  }

  @Override
  public <T> void log(
      Audit audit,
      User user,
      Operation operation,
      ValintaResource valintaResource,
      String targetOid,
      Changes changes) {
    AuditLog.log(audit, user, operation, valintaResource, targetOid, changes);
  }
}
