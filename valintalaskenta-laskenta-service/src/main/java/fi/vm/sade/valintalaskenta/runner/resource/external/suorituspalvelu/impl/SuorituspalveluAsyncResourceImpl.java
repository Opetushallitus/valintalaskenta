package fi.vm.sade.valintalaskenta.runner.resource.external.suorituspalvelu.impl;

import com.google.gson.reflect.TypeToken;
import fi.vm.sade.valintalaskenta.domain.dto.SuorituspalveluValintadataDTO;
import fi.vm.sade.valintalaskenta.runner.resource.external.RunnerRestCasClient;
import fi.vm.sade.valintalaskenta.runner.resource.external.UrlConfiguration;
import fi.vm.sade.valintalaskenta.runner.resource.external.koostepalvelu.impl.LahtotiedotException;
import fi.vm.sade.valintalaskenta.runner.resource.external.suorituspalvelu.SuorituspalveluAsyncResource;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

@Service
public class SuorituspalveluAsyncResourceImpl implements SuorituspalveluAsyncResource {

  private final RunnerRestCasClient httpClient;

  private final UrlConfiguration urlConfiguration;

  @Autowired
  public SuorituspalveluAsyncResourceImpl(
      @Qualifier("SuorituspalveluCasClient") RunnerRestCasClient httpClient) {
    this.httpClient = httpClient;
    this.urlConfiguration = UrlConfiguration.getInstance();
  }

  @Override
  public SuorituspalveluValintadataDTO haeValintaData(String hakuOid, String hakukohdeOid) {
    Map<String, Object> bodyMap = new HashMap<>();
    bodyMap.put("hakuOid", hakuOid);
    bodyMap.put("hakukohdeOid", hakukohdeOid);
    bodyMap.put("hakemusOids", java.util.Collections.emptyList());

    try {
      return httpClient
          .post(
              this.urlConfiguration.url("suorituspalvelu.valintalaskenta.valintadata"),
              new TypeToken<SuorituspalveluValintadataDTO>() {},
              bodyMap,
              Collections.emptyMap(),
              20 * 60 * 1000)
          .get();
    } catch (Exception e) {
      throw new LahtotiedotException(e); // Todo, oma erottuva virheenkäsittely
    }
  }
}
