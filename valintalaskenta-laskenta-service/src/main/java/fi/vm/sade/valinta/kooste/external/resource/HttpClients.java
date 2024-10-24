package fi.vm.sade.valinta.kooste.external.resource;

import static fi.vm.sade.valinta.sharedutils.http.HttpResource.CSRF_VALUE;

import fi.vm.sade.javautils.nio.cas.CasConfig;
import fi.vm.sade.valinta.kooste.external.resource.tarjonta.impl.TarjontaAsyncResourceImpl;
import fi.vm.sade.valinta.sharedutils.http.DateDeserializer;
import java.net.CookieManager;
import java.time.Duration;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;

@Configuration
public class HttpClients {
  public static final String CALLER_ID = "1.2.246.562.10.00000000001.valintalaskentakoostepalvelu";

  @Bean
  public CookieManager getCookieManager() {
    return new CookieManager();
  }

  @Profile({"default", "dev"})
  @Bean(name = "KoostepalveluCasClient")
  @Autowired
  public RestCasClient getKoostepalveluCasClient(
      @Value("${cas.service.koostepalvelu}") String service,
      @Value("${valintalaskentakoostepalvelu.app.username.to.ataru}") String username,
      @Value("${valintalaskentakoostepalvelu.app.password.to.ataru}") String password) {
    String ticketsUrl = UrlConfiguration.getInstance().url("cas.tickets");
    return new RestCasClient(
        new CasConfig.CasConfigBuilder(
            username, password, ticketsUrl, service, CSRF_VALUE, CALLER_ID, "")
            .setJsessionName("JSESSIONID")
            .build());
  }

  @Bean(name = "TarjontaHttpClient")
  @Autowired
  public HttpClient getTarjontaHttpClient(CookieManager cookieManager) {
    return new HttpClient(
        defaultHttpClientBuilder(cookieManager).build(), TarjontaAsyncResourceImpl.getGson());
  }

  @Profile({"default", "dev"})
  @Bean(name = "HakukohderyhmapalveluCasClient")
  @Autowired
  public RestCasClient getHakukohderyhmapalveluCasClient(
      @Value("${valintalaskentakoostepalvelu.app.username.to.hakukohderyhmapalvelu}")
          String username,
      @Value("${valintalaskentakoostepalvelu.app.password.to.hakukohderyhmapalvelu}")
          String password) {
    String ticketsUrl = UrlConfiguration.getInstance().url("cas.tickets");
    String service = UrlConfiguration.getInstance().url("hakukohderyhmapalvelu.auth.login");
    return new RestCasClient(
        new CasConfig.CasConfigBuilder(
                username, password, ticketsUrl, service, CSRF_VALUE, CALLER_ID, "")
            .setJsessionName("ring-session")
            .build());
  }

  @Profile({"default", "dev"})
  @Bean(name = "KoutaCasClient")
  @Autowired
  public RestCasClient getKoutaCasClient(
      @Value("${valintalaskentakoostepalvelu.app.username.to.kouta-internal}") String username,
      @Value("${valintalaskentakoostepalvelu.app.password.to.kouta-internal}") String password) {
    String ticketsUrl = UrlConfiguration.getInstance().url("cas.tickets");
    String service = UrlConfiguration.getInstance().url("kouta-internal.auth.login");
    return new RestCasClient(
        new CasConfig.CasConfigBuilder(
                username, password, ticketsUrl, service, CSRF_VALUE, CALLER_ID, "")
            .setJsessionName("session")
            .build());
  }

  @Bean(name = "OrganisaatioHttpClient")
  @Autowired
  public HttpClient getOrganisaatioHttpClient(CookieManager cookieManager) {
    return new HttpClient(
        defaultHttpClientBuilder(cookieManager).build(), DateDeserializer.gsonBuilder().create());
  }

  @Profile({"default", "dev"})
  @Bean(name = "ValintaperusteetCasClient")
  @Autowired
  public RestCasClient getValintaperusteetCasClient(
      @Value("${cas.service.valintaperusteet-service}") String service,
      @Value("${valintalaskentakoostepalvelu.app.username.to.valintatieto}") String username,
      @Value("${valintalaskentakoostepalvelu.app.password.to.valintatieto}") String password) {
    String ticketsUrl = UrlConfiguration.getInstance().url("cas.tickets");
    return new RestCasClient(
        new CasConfig.CasConfigBuilder(
                username, password, ticketsUrl, service, CSRF_VALUE, CALLER_ID, "/j_spring_cas_security_check")
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
