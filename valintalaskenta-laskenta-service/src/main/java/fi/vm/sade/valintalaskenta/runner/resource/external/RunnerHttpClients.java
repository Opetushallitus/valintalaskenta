package fi.vm.sade.valintalaskenta.runner.resource.external;

import static fi.vm.sade.valinta.sharedutils.http.HttpResource.CSRF_VALUE;

import fi.vm.sade.javautils.nio.cas.CasConfig;
import fi.vm.sade.valinta.sharedutils.http.DateDeserializer;
import fi.vm.sade.valintalaskenta.runner.resource.external.tarjonta.impl.TarjontaAsyncResourceImpl;
import java.net.CookieManager;
import java.time.Duration;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;

@Configuration
public class RunnerHttpClients {
  public static final String CALLER_ID = "1.2.246.562.10.00000000001.valintalaskentakoostepalvelu";

  @Bean
  public CookieManager getCookieManager() {
    return new CookieManager();
  }

  @Profile({"default", "dev"})
  @Bean(name = "KoostepalveluCasClient")
  public RunnerRestCasClient getKoostepalveluCasClient(
      @Value("${cas.service.koostepalvelu}") String service,
      @Value("${valintalaskentakoostepalvelu.app.username.to.valintatieto}") String username,
      @Value("${valintalaskentakoostepalvelu.app.password.to.valintatieto}") String password) {
    String ticketsUrl = UrlConfiguration.getInstance().url("cas.tickets");
    return new RunnerRestCasClient(
        new CasConfig.CasConfigBuilder(
                username, password, ticketsUrl, service, CSRF_VALUE, CALLER_ID, "")
            .setJsessionName("JSESSIONID")
            .build());
  }

  @Profile({"default", "dev"})
  @Bean(name = "SuorituspalveluCasClient")
  public RunnerRestCasClient getSuorituspalveluCasClient(
      @Value("${cas.service.koostepalvelu}") String service,
      @Value("${valintalaskentakoostepalvelu.app.username.to.valintatieto}")
          String username, // Todo, lisätään omat konffiarvot Supa-tunnukselle
      @Value("${valintalaskentakoostepalvelu.app.password.to.valintatieto}") String password) {
    String ticketsUrl = UrlConfiguration.getInstance().url("cas.tickets");
    return new RunnerRestCasClient(
        new CasConfig.CasConfigBuilder(
                username, password, ticketsUrl, service, CSRF_VALUE, CALLER_ID, "")
            .setJsessionName("JSESSIONID")
            .build());
  }

  @Bean(name = "TarjontaHttpClient")
  public RunnerHttpClient getTarjontaHttpClient(CookieManager cookieManager) {
    return new RunnerHttpClient(
        defaultHttpClientBuilder(cookieManager).build(), TarjontaAsyncResourceImpl.getGson());
  }

  @Profile({"default", "dev"})
  @Bean(name = "HakukohderyhmapalveluCasClient")
  public RunnerRestCasClient getHakukohderyhmapalveluCasClient(
      @Value("${valintalaskentakoostepalvelu.app.username.to.hakukohderyhmapalvelu}")
          String username,
      @Value("${valintalaskentakoostepalvelu.app.password.to.hakukohderyhmapalvelu}")
          String password) {
    String ticketsUrl = UrlConfiguration.getInstance().url("cas.tickets");
    String service = UrlConfiguration.getInstance().url("hakukohderyhmapalvelu.auth.login");
    return new RunnerRestCasClient(
        new CasConfig.CasConfigBuilder(
                username, password, ticketsUrl, service, CSRF_VALUE, CALLER_ID, "")
            .setJsessionName("ring-session")
            .build());
  }

  @Profile({"default", "dev"})
  @Bean(name = "KoutaCasClient")
  public RunnerRestCasClient getKoutaCasClient(
      @Value("${valintalaskentakoostepalvelu.app.username.to.kouta-internal}") String username,
      @Value("${valintalaskentakoostepalvelu.app.password.to.kouta-internal}") String password) {
    String ticketsUrl = UrlConfiguration.getInstance().url("cas.tickets");
    String service = UrlConfiguration.getInstance().url("kouta-internal.auth.login");
    return new RunnerRestCasClient(
        new CasConfig.CasConfigBuilder(
                username, password, ticketsUrl, service, CSRF_VALUE, CALLER_ID, "")
            .setJsessionName("session")
            .build());
  }

  @Bean(name = "OrganisaatioHttpClient")
  public RunnerHttpClient getOrganisaatioHttpClient(CookieManager cookieManager) {
    return new RunnerHttpClient(
        defaultHttpClientBuilder(cookieManager).build(), DateDeserializer.gsonBuilder().create());
  }

  @Profile({"default", "dev"})
  @Bean(name = "ValintaperusteetCasClient")
  public RunnerRestCasClient getValintaperusteetCasClient(
      @Value("${cas.service.valintaperusteet-service}") String service,
      @Value("${valintalaskentakoostepalvelu.app.username.to.valintatieto}") String username,
      @Value("${valintalaskentakoostepalvelu.app.password.to.valintatieto}") String password) {
    String ticketsUrl = UrlConfiguration.getInstance().url("cas.tickets");
    return new RunnerRestCasClient(
        new CasConfig.CasConfigBuilder(
                username,
                password,
                ticketsUrl,
                service,
                CSRF_VALUE,
                CALLER_ID,
                "/j_spring_cas_security_check")
            .setJsessionName("JSESSIONID")
            .build());
  }

  public static java.net.http.HttpClient.Builder defaultHttpClientBuilder(
      CookieManager cookieManager) {
    return java.net.http.HttpClient.newBuilder()
        .version(java.net.http.HttpClient.Version.HTTP_1_1)
        .connectTimeout(Duration.ofSeconds(10))
        .cookieHandler(cookieManager);
  }
}
