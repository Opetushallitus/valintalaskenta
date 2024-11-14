package fi.vm.sade.valintalaskenta.tulos.resource.external.impl;

import static fi.vm.sade.valintalaskenta.tulos.RestClientUtil.get;

import com.google.gson.reflect.TypeToken;
import fi.vm.sade.javautils.nio.cas.CasClient;
import fi.vm.sade.service.valintaperusteet.dto.ValintaperusteetDTO;
import fi.vm.sade.valintalaskenta.tulos.resource.external.ValintaperusteetResource;
import java.util.List;
import java.util.Map;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public class ValintaperusteetResourceImpl implements ValintaperusteetResource {

  private final CasClient casClient;
  private final String valintaperusteetBaseUrl;

  private Integer clientConnectionTimeout;
  private Integer clientReceiveTimeout;

  public ValintaperusteetResourceImpl(
      @Qualifier("valintaperusteetCasClient") final CasClient casClient,
      @Value("${valintalaskentakoostepalvelu.valintaperusteet.ilb.url}")
          final String valintaperusteetBaseUrl,
      @Value("${valintalaskenta-laskenta-service.global.http.connectionTimeoutMillis:59999}")
          final int clientConnectionTimeout,
      @Value("${valintalaskenta-laskenta-service.global.http.receiveTimeoutMillis:1799999}")
          final int clientReceiveTimeout) {
    this.casClient = casClient;
    this.valintaperusteetBaseUrl = valintaperusteetBaseUrl;
    this.clientConnectionTimeout = clientConnectionTimeout;
    this.clientReceiveTimeout = clientReceiveTimeout;
  }

  @Override
  public List<ValintaperusteetDTO> haeValintaperusteet(
      final String hakukohdeOid, final Integer vaihe) {
    final TypeToken<List<ValintaperusteetDTO>> typeToken = new TypeToken<>() {};
    final Map<String, List<String>> queryParams =
        vaihe != null ? Map.of("vaihe", List.of(vaihe.toString())) : null;
    return get(
        this.casClient,
        String.format("%s/valintaperusteet/%s", valintaperusteetBaseUrl, hakukohdeOid),
        typeToken,
        queryParams,
        clientConnectionTimeout,
        clientReceiveTimeout);
  }
}
