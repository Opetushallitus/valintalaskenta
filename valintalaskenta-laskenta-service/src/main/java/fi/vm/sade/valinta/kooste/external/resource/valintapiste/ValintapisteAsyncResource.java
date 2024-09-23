package fi.vm.sade.valinta.kooste.external.resource.valintapiste;

import fi.vm.sade.valinta.kooste.external.resource.valintapiste.dto.PisteetWithLastModified;
import fi.vm.sade.valinta.kooste.AuditSession;
import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface ValintapisteAsyncResource {
  String LAST_MODIFIED = "Last-Modified";

  CompletableFuture<PisteetWithLastModified> getValintapisteetWithHakemusOidsAsFuture(
      List<String> hakemusOIDs, AuditSession auditSession);
}
