package fi.vm.sade.valinta.kooste.external.resource.kouta;

import java.util.concurrent.CompletableFuture;

public interface KoutaAsyncResource {
  CompletableFuture<KoutaHakukohde> haeHakukohde(String hakukohdeOid);
}
