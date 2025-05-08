package fi.vm.sade.valintalaskenta.runner.resource.external.koostepalvelu.impl;

import com.google.gson.reflect.TypeToken;
import fi.vm.sade.valintalaskenta.domain.dto.LaskeDTO;
import fi.vm.sade.valintalaskenta.domain.dto.seuranta.LaskentaDto;
import fi.vm.sade.valintalaskenta.runner.resource.external.RunnerRestCasClient;
import fi.vm.sade.valintalaskenta.runner.resource.external.UrlConfiguration;
import fi.vm.sade.valintalaskenta.runner.resource.external.koostepalvelu.KoostepalveluAsyncResource;
import java.util.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

@Service
public class KoostepalveluAsyncResourceImpl implements KoostepalveluAsyncResource {
  private final RunnerRestCasClient httpClient;

  private final UrlConfiguration urlConfiguration;

  @Autowired
  public KoostepalveluAsyncResourceImpl(
      @Qualifier("KoostepalveluCasClient") RunnerRestCasClient httpClient) {
    this.httpClient = httpClient;
    this.urlConfiguration = UrlConfiguration.getInstance();
  }

  public LaskeDTO haeLahtotiedot(
      LaskentaDto laskenta,
      String hakukohdeOid,
      boolean retryHakemuksetAndOppijat,
      boolean withHakijaRyhmat) {
    Map<String, String> parameters = new HashMap<>();
    parameters.put("uuid", laskenta.getUuid());
    parameters.put("valintakoelaskenta", laskenta.getValintakoelaskenta() + "");
    parameters.put("erillishaku", laskenta.getErillishaku() + "");
    parameters.put("retryHakemuksetAndOppijat", retryHakemuksetAndOppijat + "");
    parameters.put("withHakijaRyhmat", withHakijaRyhmat + "");
    if (laskenta.getValinnanvaihe().isPresent()) {
      parameters.put("valinnanvaihe", laskenta.getValinnanvaihe().get().toString());
    }

    try {
      return httpClient
          .get(
              this.urlConfiguration.url(
                  "valintalaskentakoostepalvelu.lahtotiedot.baseurl",
                  laskenta.getHakuOid(),
                  hakukohdeOid,
                  parameters),
              new TypeToken<LaskeDTO>() {},
              Collections.emptyMap(),
              20 * 60 * 1000)
          .get();
    } catch (Exception e) {
      throw new LahtotiedotException(e);
    }
  }
}
