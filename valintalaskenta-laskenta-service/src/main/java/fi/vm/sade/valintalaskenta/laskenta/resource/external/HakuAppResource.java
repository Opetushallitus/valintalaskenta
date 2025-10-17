package fi.vm.sade.valintalaskenta.laskenta.resource.external;

import static fi.vm.sade.valintalaskenta.tulos.RestClientUtil.post;

import com.google.gson.reflect.TypeToken;
import fi.vm.sade.javautils.nio.cas.CasClient;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public class HakuAppResource {
  private static final Logger LOG = LoggerFactory.getLogger(HakuAppResource.class);

  private final CasClient casClient;
  private final String baseUrl;
  private final Integer clientConnectionTimeout;
  private final Integer clientReceiveTimeout;

  public HakuAppResource(
      @Qualifier("hakuAppCasClient") final CasClient hakuAppCasClient,
      @Value("${valintalaskentakoostepalvelu.haku-app.rest.url}") final String hakuAppBaseUrl,
      @Value("${valintalaskenta-laskenta-service.global.http.connectionTimeoutMillis:59999}")
          Integer clientConnectionTimeout,
      @Value("${valintalaskenta-laskenta-service.global.http.receiveTimeoutMillis:1799999}")
          Integer clientReceiveTimeout) {
    this.casClient = hakuAppCasClient;
    this.baseUrl = hakuAppBaseUrl;
    this.clientConnectionTimeout = clientConnectionTimeout;
    this.clientReceiveTimeout = clientReceiveTimeout;
  }

  public List<HakuAppHakemus> getHakemukset(String hakuOid, String hakukohdeOid) {
    Map<String, List<String>> requestBody = new HashMap<>();
    requestBody.put("states", Arrays.asList("ACTIVE", "INCOMPLETE"));
    requestBody.put("asIds", Collections.singletonList(hakuOid));
    requestBody.put("aoOids", Collections.singletonList(hakukohdeOid));
    requestBody.put("keys", Arrays.asList("oid", "personOid"));

    LOG.info(
        "Haetaan hakemukset haku-appista haulle {} ja hakukohteelle {}", hakuOid, hakukohdeOid);

    List<HakuAppHakemus> hakemukset =
        post(
            casClient,
            String.format("%s/applications/listfull", baseUrl),
            new TypeToken<>() {},
            requestBody,
            clientConnectionTimeout,
            clientReceiveTimeout);

    LOG.info("Haku-appista saatiin {} hakemusta", hakemukset.size());
    return hakemukset;
  }
}
