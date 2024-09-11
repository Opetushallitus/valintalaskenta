package fi.vm.sade.valinta.kooste.external.resource.kouta.impl;

import com.google.gson.reflect.TypeToken;
import fi.vm.sade.valinta.kooste.external.resource.kouta.KoutaAsyncResource;
import fi.vm.sade.valinta.kooste.external.resource.kouta.KoutaHakukohde;
import fi.vm.sade.valinta.kooste.external.resource.kouta.dto.KoutaHakukohdeDTO;
import fi.vm.sade.valinta.kooste.external.resource.viestintapalvelu.RestCasClient;
import fi.vm.sade.valinta.kooste.url.UrlConfiguration;
import java.util.Collections;
import java.util.concurrent.CompletableFuture;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

@Service
public class KoutaAsyncResourceImpl implements KoutaAsyncResource {
  private final UrlConfiguration urlConfiguration = UrlConfiguration.getInstance();
  private final RestCasClient koutaClient;

  @Autowired
  public KoutaAsyncResourceImpl(@Qualifier("KoutaCasClient") RestCasClient koutaClient) {
    this.koutaClient = koutaClient;
  }

  @Override
  public CompletableFuture<KoutaHakukohde> haeHakukohde(String hakukohdeOid) {
    CompletableFuture<KoutaHakukohdeDTO> koutaF =
        this.koutaClient.get(
            urlConfiguration.url("kouta-internal.hakukohde.hakukohdeoid", hakukohdeOid),
            new TypeToken<KoutaHakukohdeDTO>() {},
            Collections.emptyMap(),
            10 * 1000);
    return koutaF.thenApplyAsync(KoutaHakukohde::new);
  }
}
