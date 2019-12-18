package fi.vm.sade.valintalaskenta.tulos.logging;

import fi.vm.sade.auditlog.Audit;
import fi.vm.sade.auditlog.Changes;
import fi.vm.sade.auditlog.Operation;
import fi.vm.sade.auditlog.User;
import fi.vm.sade.valinta.sharedutils.ValintaResource;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

public interface LaskentaAuditLog {
    User getUser(HttpServletRequest request);

    <T> void log(Audit audit, User user, Operation operation, ValintaResource valintaResource, String targetOid, Changes changes, Map<String, String> additionalInfo);

    <T> void log(Audit audit, User user, Operation operation, ValintaResource valintaResource, String targetOid, Changes changes);
}
