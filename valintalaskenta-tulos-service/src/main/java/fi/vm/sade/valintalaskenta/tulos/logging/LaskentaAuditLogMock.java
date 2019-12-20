package fi.vm.sade.valintalaskenta.tulos.logging;

import fi.vm.sade.auditlog.Audit;
import fi.vm.sade.auditlog.Changes;
import fi.vm.sade.auditlog.Operation;
import fi.vm.sade.auditlog.User;
import fi.vm.sade.valinta.sharedutils.ValintaResource;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

public class LaskentaAuditLogMock implements LaskentaAuditLog {

    @Override
    public User getUser(HttpServletRequest request) {
        return new User(null, "mock-session", "mock-user-agent");
    }

    @Override
    public <T> void log(Audit audit, User user, Operation operation, ValintaResource valintaResource, String targetOid, Changes changes, Map<String, String> additionalInfo) {
    }

    @Override
    public <T> void log(Audit audit, User user, Operation operation, ValintaResource valintaResource, String targetOid, Changes changes) {
    }
}
