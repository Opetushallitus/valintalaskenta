package fi.vm.sade.valintalaskenta.laskenta.resource.external;

import static fi.vm.sade.valintalaskenta.tulos.RestClientUtil.get;

import com.google.gson.reflect.TypeToken;
import fi.vm.sade.javautils.nio.cas.CasClient;
import java.util.List;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public class AtaruResource {
  private static final Logger LOG = LoggerFactory.getLogger(AtaruResource.class);

  private final CasClient casClient;
  private final String baseUrl;
  private final Integer clientConnectionTimeout;
  private final Integer clientReceiveTimeout;

  public AtaruResource(
      @Qualifier("ataruCasClient") final CasClient ataruCasClient,
      @Value("${valintalaskentakoostepalvelu.ataru.rest.url}") String baseUrl,
      @Value("${valintalaskenta-laskenta-service.global.http.connectionTimeoutMillis:59999}")
          Integer clientConnectionTimeout,
      @Value("${valintalaskenta-laskenta-service.global.http.receiveTimeoutMillis:1799999}")
          Integer clientReceiveTimeout) {

    this.casClient = ataruCasClient;
    this.baseUrl = baseUrl;
    this.clientConnectionTimeout = clientConnectionTimeout;
    this.clientReceiveTimeout = clientReceiveTimeout;
  }

  public List<AtaruHakemus> getHakemukset(String hakuOid, String hakukohdeOid) {
    LOG.info(
        "Haetaan hakemusten tiedot Atarusta haulle {} ja hakukohteelle {}", hakuOid, hakukohdeOid);

    List<AtaruHakemus> hakemukset =
        get(
            casClient,
            String.format("%s/valintapiste", baseUrl),
            new TypeToken<>() {},
            Map.of("hakuOid", List.of(hakuOid), "hakukohdeOid", List.of(hakukohdeOid)),
            clientConnectionTimeout,
            clientReceiveTimeout);

    LOG.info("Atarusta saatiin {} hakemusta", hakemukset.size());
    return hakemukset;
  }
}
