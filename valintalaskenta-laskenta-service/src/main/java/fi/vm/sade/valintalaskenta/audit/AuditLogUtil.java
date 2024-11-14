package fi.vm.sade.valintalaskenta.audit;

import fi.vm.sade.auditlog.ApplicationType;
import fi.vm.sade.auditlog.Audit;
import fi.vm.sade.auditlog.Changes;
import fi.vm.sade.valinta.sharedutils.AuditLog;
import fi.vm.sade.valinta.sharedutils.AuditLogger;
import fi.vm.sade.valinta.sharedutils.ValintaResource;
import fi.vm.sade.valinta.sharedutils.ValintaperusteetOperation;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

public class AuditLogUtil {

  public static final Audit AUDIT =
      new Audit(new AuditLogger(), "valintalaskentakoostepalvelu", ApplicationType.VIRKAILIJA);

  public static void auditLogLaskenta(AuditSession auditSession, ValintaperusteetOperation operation, String uuid, String hakuOid, Collection<String> hakukohteet, Optional<String> tyyppi) {
    Map<String, String> additionalAuditInfo = new HashMap<>();
    additionalAuditInfo.put("uuid", uuid);
    if(tyyppi.isPresent()) {
      additionalAuditInfo.put("tyyppi", tyyppi.get());
    }
    additionalAuditInfo.put(
        "hakukohteet",
        hakukohteet.stream()
            .collect(Collectors.toList())
            .toString());
    AuditLog.log(
        AUDIT,
        auditSession.asAuditUser(),
        operation,
        ValintaResource.LASKENTATOTEUTUS,
        hakuOid,
        Changes.EMPTY,
        additionalAuditInfo);
  }

}
