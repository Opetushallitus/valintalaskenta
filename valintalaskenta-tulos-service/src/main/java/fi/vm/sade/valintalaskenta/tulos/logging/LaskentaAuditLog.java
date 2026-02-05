package fi.vm.sade.valintalaskenta.tulos.logging;

import fi.vm.sade.auditlog.Audit;
import fi.vm.sade.auditlog.Changes;
import fi.vm.sade.auditlog.Operation;
import fi.vm.sade.auditlog.Target;
import fi.vm.sade.auditlog.User;
import fi.vm.sade.valinta.sharedutils.ValintaResource;
import jakarta.servlet.http.HttpServletRequest;
import java.util.Map;

public interface LaskentaAuditLog {
  User getUser(HttpServletRequest request);

  void log(
      Audit audit,
      User user,
      Operation operation,
      ValintaResource valintaResource,
      String targetOid,
      Changes changes,
      Map<String, String> additionalInfo);

  default void log(
      Audit audit,
      HttpServletRequest request,
      Operation operation,
      ValintaResource valintaResource,
      String targetOid,
      Changes changes,
      Map<String, String> additionalInfo) {
    log(audit, getUser(request), operation, valintaResource, targetOid, changes, additionalInfo);
  }

  void log(
      Audit audit,
      User user,
      Operation operation,
      ValintaResource valintaResource,
      String targetOid,
      Changes changes);

  default void log(
      Audit audit,
      HttpServletRequest request,
      Operation operation,
      ValintaResource valintaResource,
      String targetOid,
      Changes changes) {
    log(audit, getUser(request), operation, valintaResource, targetOid, changes);
  }

  void log(Audit audit, User user, Operation operation, Target target, Changes changes);

  default void log(
      Audit audit,
      HttpServletRequest request,
      Operation operation,
      Target target,
      Changes changes) {
    log(audit, getUser(request), operation, target, changes);
  }
}
