package fi.vm.sade.valintalaskenta.testing;

import fi.vm.sade.valinta.kooste.external.resource.RestCasClient;
import org.asynchttpclient.AsyncHttpClient;
import org.asynchttpclient.Dsl;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class KoosteTestProfileConfiguration {
  private static final AsyncHttpClient asyncHttpClient = Dsl.asyncHttpClient();
  private static RestCasClient REST_CAS_CLIENT =
      new RestCasClient(
          request -> asyncHttpClient.executeRequest(request).toCompletableFuture()) {};

  @Bean(name = "HakukohderyhmapalveluCasClient")
  public RestCasClient getHakukohderyhmapalveluCasClient() {
    return REST_CAS_CLIENT;
  }

  @Bean(name = "KoutaCasClient")
  public RestCasClient getKoutaCasClient() {
    return REST_CAS_CLIENT;
  }

  @Bean(name = "ValintalaskentaCasClient")
  public RestCasClient getValintalaskentaCasClient() {
    return REST_CAS_CLIENT;
  }

  @Bean(name = "ValintaperusteetCasClient")
  public RestCasClient getValintaperusteetCasClient() {
    return REST_CAS_CLIENT;
  }

  @Bean(name = "KoostepalveluCasClient")
  public RestCasClient getKoostepalveluCasClient() {
    return REST_CAS_CLIENT;
  }
}
