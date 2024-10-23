package fi.vm.sade.valinta.kooste.external.resource.koostepalvelu.impl;

import com.google.gson.reflect.TypeToken;
import fi.vm.sade.valinta.kooste.external.resource.RestCasClient;
import fi.vm.sade.valinta.kooste.external.resource.UrlConfiguration;
import fi.vm.sade.valinta.kooste.external.resource.koostepalvelu.KoostepalveluAsyncResource;
import fi.vm.sade.valintalaskenta.domain.dto.LaskeDTO;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.LaskentaDto;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.concurrent.CompletableFuture;

@Service
public class KoostepalveluAsyncResourceImpl implements KoostepalveluAsyncResource {
  private static final Logger LOG =
      LoggerFactory.getLogger(KoostepalveluAsyncResourceImpl.class);
  private final RestCasClient httpClient;

  private final UrlConfiguration urlConfiguration;

  @Autowired
  public KoostepalveluAsyncResourceImpl(
      @Qualifier("KoostepalveluCasClient") RestCasClient httpClient) {
    this.httpClient = httpClient;
    this.urlConfiguration = UrlConfiguration.getInstance();
  }

  /*

      @RequestParam(value = "uuid") String uuid,
      @RequestParam(value = "erillishaku", required = false) Boolean erillishaku,
      @RequestParam(value = "valinnanvaihe", required = false) Integer valinnanvaihe,
      @RequestParam(value = "valintakoelaskenta", required = false) Boolean valintakoelaskenta,
      @RequestParam(value = "retryHakemuksetAndOppijat") Boolean retryHakemuksetAndOppijat,
      @RequestParam(value = "withHakijaRyhmat") Boolean withHakijaRyhmat


   */

  public CompletableFuture<LaskeDTO> haeLahtotiedot(
      LaskentaDto laskenta,
      String hakukohdeOid,
      boolean retryHakemuksetAndOppijat,
      boolean withHakijaRyhmat) {
    return httpClient.get(
        this.urlConfiguration.url(
            "valintalaskentakoostepalvelu.lahtotiedot.baseurl",
            laskenta.getHakuOid(), hakukohdeOid),
        new TypeToken<LaskeDTO>() {},
        Collections.emptyMap(),
        60 * 60 * 1000);
  }
}
