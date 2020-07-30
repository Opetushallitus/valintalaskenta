package fi.vm.sade.valintalaskenta.tulos.logging;

import fi.vm.sade.auditlog.Audit;
import fi.vm.sade.auditlog.Changes;
import fi.vm.sade.auditlog.Operation;
import fi.vm.sade.auditlog.User;
import fi.vm.sade.valinta.sharedutils.ValintaResource;
import java.util.Map;
import javax.servlet.http.HttpServletRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class LaskentaAuditLogMock implements LaskentaAuditLog {
  private static final Logger LOG = LoggerFactory.getLogger(LaskentaAuditLogMock.class);

  @Override
  public User getUser(HttpServletRequest request) {
    LOG.info("Mock audit user");
    return new User(null, "mock-session", "mock-user-agent");
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
    LOG.info(
        "Mock audit log called with {}, {}, {}, {}, {}, {}",
        user,
        operation,
        valintaResource,
        targetOid,
        changes,
        additionalInfo);
  }

  @Override
  public <T> void log(
      Audit audit,
      User user,
      Operation operation,
      ValintaResource valintaResource,
      String targetOid,
      Changes changes) {
    LOG.info(
        "Mock audit log called with {}, {}, {}, {}, {}",
        user,
        operation,
        valintaResource,
        targetOid,
        changes);
  }
}
