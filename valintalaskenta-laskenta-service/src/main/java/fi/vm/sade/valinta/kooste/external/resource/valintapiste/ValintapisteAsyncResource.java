package fi.vm.sade.valinta.kooste.external.resource.valintapiste;

import fi.vm.sade.valinta.kooste.external.resource.valintapiste.dto.PisteetWithLastModified;
import fi.vm.sade.valinta.kooste.external.resource.valintapiste.dto.Valintapisteet;
import fi.vm.sade.valinta.kooste.external.resource.valintatulosservice.dto.AuditSession;
import io.reactivex.Observable;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.CompletableFuture;

public interface ValintapisteAsyncResource {
  String LAST_MODIFIED = "Last-Modified";
  String IF_UNMODIFIED_SINCE = "If-Unmodified-Since";

  CompletableFuture<PisteetWithLastModified> getValintapisteet(
      String hakuOID, String hakukohdeOID, AuditSession auditSession);

  Observable<PisteetWithLastModified> getValintapisteet(
      Collection<String> hakemusOIDs, AuditSession auditSession);

  CompletableFuture<PisteetWithLastModified> getValintapisteetWithHakemusOidsAsFuture(
      List<String> hakemusOIDs, AuditSession auditSession);

  CompletableFuture<Set<String>> putValintapisteet(
      Optional<String> ifUnmodifiedSince, List<Valintapisteet> pisteet, AuditSession auditSession);
}
