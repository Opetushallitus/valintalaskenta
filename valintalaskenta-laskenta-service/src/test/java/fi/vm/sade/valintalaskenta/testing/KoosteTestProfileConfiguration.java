package fi.vm.sade.valintalaskenta.testing;

import fi.vm.sade.valintalaskenta.runner.resource.external.RunnerRestCasClient;
import org.asynchttpclient.AsyncHttpClient;
import org.asynchttpclient.Dsl;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class KoosteTestProfileConfiguration {
  private static final AsyncHttpClient asyncHttpClient = Dsl.asyncHttpClient();
  private static RunnerRestCasClient REST_CAS_CLIENT =
      new RunnerRestCasClient(
          request -> asyncHttpClient.executeRequest(request).toCompletableFuture()) {};

  @Bean(name = "HakukohderyhmapalveluCasClient")
  public RunnerRestCasClient getHakukohderyhmapalveluCasClient() {
    return REST_CAS_CLIENT;
  }

  @Bean(name = "KoutaCasClient")
  public RunnerRestCasClient getKoutaCasClient() {
    return REST_CAS_CLIENT;
  }

  @Bean(name = "ValintalaskentaCasClient")
  public RunnerRestCasClient getValintalaskentaCasClient() {
    return REST_CAS_CLIENT;
  }

  @Bean(name = "ValintaperusteetCasClient")
  public RunnerRestCasClient getValintaperusteetCasClient() {
    return REST_CAS_CLIENT;
  }

  @Bean(name = "KoostepalveluCasClient")
  public RunnerRestCasClient getKoostepalveluCasClient() {
    return REST_CAS_CLIENT;
  }

  @Bean(name = "SuorituspalveluCasClient")
  public RunnerRestCasClient getSuorituspalveluCasClient() {
    return REST_CAS_CLIENT;
  }
}
