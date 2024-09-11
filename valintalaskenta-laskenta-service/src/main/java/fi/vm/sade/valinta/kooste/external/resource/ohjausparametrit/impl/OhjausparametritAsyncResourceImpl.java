package fi.vm.sade.valinta.kooste.external.resource.ohjausparametrit.impl;

import com.google.gson.reflect.TypeToken;
import fi.vm.sade.valinta.kooste.external.resource.HttpClient;
import fi.vm.sade.valinta.kooste.external.resource.ohjausparametrit.OhjausparametritAsyncResource;
import fi.vm.sade.valinta.kooste.external.resource.ohjausparametrit.dto.ParametritDTO;
import fi.vm.sade.valinta.kooste.url.UrlConfiguration;
import java.time.Duration;
import java.util.concurrent.CompletableFuture;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.stereotype.Service;

@Service
@Component("OhjausparametritAsyncResourceImpl")
public class OhjausparametritAsyncResourceImpl implements OhjausparametritAsyncResource {
  private final HttpClient client;
  private final UrlConfiguration urlConfiguration;
  private final Duration requestTimeout;

  @Autowired
  public OhjausparametritAsyncResourceImpl(
      @Qualifier("OhjausparametritHttpClient") HttpClient client,
      @Value("${valintalaskentakoostepalvelu.ohjausparametrit.request.timeout.seconds:20}")
          int requestTimeoutSeconds) {
    this.client = client;
    this.urlConfiguration = UrlConfiguration.getInstance();
    this.requestTimeout = Duration.ofSeconds(requestTimeoutSeconds);
  }

  @Override
  public CompletableFuture<ParametritDTO> haeHaunOhjausparametrit(String hakuOid) {
    return this.client
        .getResponse(
            this.urlConfiguration.url("ohjausparametrit-service.parametri", hakuOid),
            this.requestTimeout,
            x -> x)
        .thenApply(
            response -> {
              if (response.statusCode() == 404) {
                return new ParametritDTO();
              }
              return this.client.parseJson(response, new TypeToken<ParametritDTO>() {}.getType());
            });
  }
}
