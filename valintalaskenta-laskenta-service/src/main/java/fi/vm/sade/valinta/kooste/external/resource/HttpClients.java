package fi.vm.sade.valinta.kooste.external.resource;

import static fi.vm.sade.valinta.sharedutils.http.HttpResource.CSRF_VALUE;

import fi.vm.sade.javautils.nio.cas.CasConfig;
import fi.vm.sade.valinta.kooste.external.resource.tarjonta.impl.TarjontaAsyncResourceImpl;
import fi.vm.sade.valinta.kooste.external.resource.valintatulosservice.impl.ValintaTulosServiceAsyncResourceImpl;
import fi.vm.sade.valinta.kooste.external.resource.viestintapalvelu.RestCasClient;
import fi.vm.sade.valinta.kooste.url.UrlConfiguration;
import fi.vm.sade.valinta.sharedutils.http.DateDeserializer;
import java.net.CookieManager;
import java.time.Duration;
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

  @Bean(name = "CasHttpClient")
  public java.net.http.HttpClient getCasHttpClient(CookieManager cookieManager) {
    return defaultHttpClientBuilder(cookieManager).build();
  }

  @Bean(name = "AtaruInternalHttpClient")
  public java.net.http.HttpClient getAtaruInternalHttpClient(CookieManager cookieManager) {
    return defaultHttpClientBuilder(cookieManager).build();
  }

  @Profile({"default", "dev"})
  @Bean(name = "AtaruCasClient")
  public RestCasClient getAtaruCasClient(
      @Value("${cas.service.ataru}") String service,
      @Value("${valintalaskentakoostepalvelu.app.username.to.ataru}") String username,
      @Value("${valintalaskentakoostepalvelu.app.password.to.ataru}") String password) {
    String ticketsUrl = UrlConfiguration.getInstance().url("cas.tickets");
    return new RestCasClient(
        new CasConfig.CasConfigBuilder(
                username, password, ticketsUrl, service, CSRF_VALUE, CALLER_ID, "")
            .setJsessionName("ring-session")
            .build());
  }

  @Bean(name = "HakuAppInternalHttpClient")
  public java.net.http.HttpClient getHakuAppInternalHttpClient(CookieManager cookieManager) {
    return defaultHttpClientBuilder(cookieManager).build();
  }

  @Bean(name = "SijoitteluServiceInternalHttpClient")
  public java.net.http.HttpClient getSijoitteluServiceInternalHttpClient(
      CookieManager cookieManager) {
    return defaultHttpClientBuilder(cookieManager).build();
  }

  @Profile({"default", "dev"})
  @Bean(name = "HakuAppCasClient")
  public RestCasClient getHakuAppCasClient(
      @Value("${cas.service.haku-service}") String service,
      @Value("${valintalaskentakoostepalvelu.app.username.to.haku}") String username,
      @Value("${valintalaskentakoostepalvelu.app.password.to.haku}") String password) {
    String ticketsUrl = UrlConfiguration.getInstance().url("cas.tickets");
    return new RestCasClient(
        new CasConfig.CasConfigBuilder(
                username, password, ticketsUrl, service, CSRF_VALUE, CALLER_ID, "")
            .setJsessionName("JSESSIONID")
            .build());
  }

  @Profile({"default", "dev"})
  @Bean(name = "SijoitteluServiceCasClient")
  public RestCasClient getSijoitteluServiceCasClient(
      @Value("${cas.service.sijoittelu-service}") String service,
      @Value("${valintalaskentakoostepalvelu.app.username.to.sijoittelu}") String username,
      @Value("${valintalaskentakoostepalvelu.app.password.to.sijoittelu}") String password) {
    String ticketsUrl = UrlConfiguration.getInstance().url("cas.tickets");
    return new RestCasClient(
        new CasConfig.CasConfigBuilder(
                username, password, ticketsUrl, service, CSRF_VALUE, CALLER_ID, "")
            .setJsessionName("JSESSIONID")
            .build());
  }

  @Bean(name = "TarjontaHttpClient")
  public HttpClient getTarjontaHttpClient(CookieManager cookieManager) {
    return new HttpClient(
        defaultHttpClientBuilder(cookieManager).build(), TarjontaAsyncResourceImpl.getGson());
  }

  @Bean(name = "HakukohderyhmapalveluInternalHttpClient")
  public java.net.http.HttpClient getHakukohderyhmapalveluInternalHttpClient(
      CookieManager cookieManager) {
    return defaultHttpClientBuilder(cookieManager).build();
  }

  @Profile({"default", "dev"})
  @Bean(name = "HakukohderyhmapalveluCasClient")
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

  @Bean(name = "KoutaInternalHttpClient")
  public java.net.http.HttpClient getKoutaInternalHttpClient(CookieManager cookieManager) {
    return defaultHttpClientBuilder(cookieManager).build();
  }

  @Profile({"default", "dev"})
  @Bean(name = "KoutaCasClient")
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

  @Bean(name = "ValintaTulosServiceHttpClient")
  public HttpClient getValintaTulosServiceHttpClient(CookieManager cookieManager) {
    return new HttpClient(
        defaultHttpClientBuilder(cookieManager).build(),
        ValintaTulosServiceAsyncResourceImpl.getGson());
  }

  @Bean(name = "OhjausparametritHttpClient")
  public HttpClient getOhjausparametritHttpClient(CookieManager cookieManager) {
    return new HttpClient(
        defaultHttpClientBuilder(cookieManager).build(), DateDeserializer.gsonBuilder().create());
  }

  @Bean(name = "OrganisaatioHttpClient")
  public HttpClient getOrganisaatioHttpClient(CookieManager cookieManager) {
    return new HttpClient(
        defaultHttpClientBuilder(cookieManager).build(), DateDeserializer.gsonBuilder().create());
  }

  @Bean(name = "DokumenttiHttpClient")
  public HttpClient getDokumenttiHttpClient(CookieManager cookieManager) {
    return new HttpClient(
        defaultHttpClientBuilder(cookieManager).build(), DateDeserializer.gsonBuilder().create());
  }

  @Bean(name = "ViestintapalveluInternalHttpClient")
  public java.net.http.HttpClient getViestintapalveluInternalHttpClient(
      CookieManager cookieManager) {
    return defaultHttpClientBuilder(cookieManager).build();
  }

  @Profile({"default", "dev"})
  @Bean(name = "ViestintapalveluCasClient")
  public RestCasClient getViestintapalveluCasClient(
      @Value("${cas.service.viestintapalvelu}") String service,
      @Value("${valintalaskentakoostepalvelu.app.username.to.valintatieto}") String username,
      @Value("${valintalaskentakoostepalvelu.app.password.to.valintatieto}") String password) {
    String ticketsUrl = UrlConfiguration.getInstance().url("cas.tickets");
    return new RestCasClient(
        new CasConfig.CasConfigBuilder(
                username, password, ticketsUrl, service, CSRF_VALUE, CALLER_ID, "")
            .setJsessionName("JSESSIONID")
            .build());
  }

  @Bean(name = "KoodistoHttpClient")
  public HttpClient getKoodistoHttpClient(CookieManager cookieManager) {
    return new HttpClient(
        defaultHttpClientBuilder(cookieManager).build(), DateDeserializer.gsonBuilder().create());
  }

  @Bean(name = "OppijanumerorekisteriInternalHttpClient")
  public java.net.http.HttpClient getOppijanumerorekisteriInternalHttpClient(
      CookieManager cookieManager) {
    return defaultHttpClientBuilder(cookieManager).build();
  }

  @Profile({"default", "dev"})
  @Bean(name = "OppijanumerorekisteriCasClient")
  public RestCasClient getOppijanumerorekisteriCasClient(
      @Value("${cas.service.oppijanumerorekisteri-service}") String service,
      @Value("${valintalaskentakoostepalvelu.app.username.to.haku}") String username,
      @Value("${valintalaskentakoostepalvelu.app.password.to.haku}") String password) {
    String ticketsUrl = UrlConfiguration.getInstance().url("cas.tickets");
    return new RestCasClient(
        new CasConfig.CasConfigBuilder(
                username, password, ticketsUrl, service, CSRF_VALUE, CALLER_ID, "")
            .setJsessionName("JSESSIONID")
            .build());
  }

  @Bean(name = "ValintapisteServiceInternalHttpClient")
  public java.net.http.HttpClient getValintapisteServiceInternalHttpClient(
      CookieManager cookieManager) {
    return defaultHttpClientBuilder(cookieManager).build();
  }

  @Profile({"default", "dev"})
  @Bean(name = "ValintapisteServiceCasClient")
  public RestCasClient getValintapisteServiceCasClient(
      @Value("${cas.service.valintapiste-service}") String service,
      @Value("${valintalaskentakoostepalvelu.app.username.to.valintatieto}") String username,
      @Value("${valintalaskentakoostepalvelu.app.password.to.valintatieto}") String password) {
    String ticketsUrl = UrlConfiguration.getInstance().url("cas.tickets");
    return new RestCasClient(
        new CasConfig.CasConfigBuilder(
                username, password, ticketsUrl, service, CSRF_VALUE, CALLER_ID, "")
            .setJsessionName("ring-session")
            .build());
  }

  @Bean(name = "ValintalaskentaValintakoeHttpClient")
  public HttpClient getValintalaskentaValintakoeHttpClient(CookieManager cookieManager) {
    return new HttpClient(
        defaultHttpClientBuilder(cookieManager).build(), DateDeserializer.gsonBuilder().create());
  }

  @Bean(name = "SuoritusrekisteriInternalHttpClient")
  public java.net.http.HttpClient getSuoritusrekisteriInternalHttpClient(
      CookieManager cookieManager) {
    return defaultHttpClientBuilder(cookieManager).build();
  }

  @Profile({"default", "dev"})
  @Bean(name = "SuoritusrekisteriCasClient")
  public RestCasClient getSuoritusrekisteriCasClient(
      @Value("${cas.service.suoritusrekisteri}") String service,
      @Value("${valintalaskentakoostepalvelu.app.username.to.valintatieto}") String username,
      @Value("${valintalaskentakoostepalvelu.app.password.to.valintatieto}") String password) {
    String ticketsUrl = UrlConfiguration.getInstance().url("cas.tickets");
    return new RestCasClient(
        new CasConfig.CasConfigBuilder(
                username, password, ticketsUrl, service, CSRF_VALUE, CALLER_ID, "")
            .setJsessionName("JSESSIONID")
            .build());
  }

  @Bean(name = "ValintalaskentaInternalHttpClient")
  public java.net.http.HttpClient getValintalaskentaInternalHttpClient(
      CookieManager cookieManager) {
    return defaultHttpClientBuilder(cookieManager).build();
  }

  @Profile({"default", "dev"})
  @Bean(name = "ValintalaskentaCasClient")
  public RestCasClient getValintalaskentaCasClient(
      @Value("${cas.service.valintalaskenta-service}") String service,
      @Value("${valintalaskentakoostepalvelu.app.username.to.valintatieto}") String username,
      @Value("${valintalaskentakoostepalvelu.app.password.to.valintatieto}") String password) {
    String ticketsUrl = UrlConfiguration.getInstance().url("cas.tickets");
    return new RestCasClient(
        new CasConfig.CasConfigBuilder(
                username, password, ticketsUrl, service, CSRF_VALUE, CALLER_ID, "")
            .setJsessionName("JSESSIONID")
            .build(),
        DateDeserializer.gsonBuilder().setDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'").create());
  }

  @Bean(name = "ValintaperusteetInternalHttpClient")
  public java.net.http.HttpClient getValintaperusteetInternalHttpClient(
      CookieManager cookieManager) {
    return defaultHttpClientBuilder(cookieManager).build();
  }

  @Profile({"default", "dev"})
  @Bean(name = "ValintaperusteetCasClient")
  public RestCasClient getValintaperusteetCasClient(
      @Value("${cas.service.valintaperusteet-service}") String service,
      @Value("${valintalaskentakoostepalvelu.app.username.to.valintatieto}") String username,
      @Value("${valintalaskentakoostepalvelu.app.password.to.valintatieto}") String password) {
    String ticketsUrl = UrlConfiguration.getInstance().url("cas.tickets");
    return new RestCasClient(
        new CasConfig.CasConfigBuilder(
                username, password, ticketsUrl, service, CSRF_VALUE, CALLER_ID, "")
            .setJsessionName("JSESSIONID")
            .build());
  }

  @Profile({"default", "dev"})
  @Bean(name = "ryhmasahkopostiCasClient")
  public RestCasClient getRyhmasahkopostiCasClient(
      @Value("${cas.service.ryhmasahkoposti-service}") String service,
      @Value("${valintalaskentakoostepalvelu.app.username.to.valintatieto}") String username,
      @Value("${valintalaskentakoostepalvelu.app.password.to.valintatieto}") String password) {
    String ticketsUrl = UrlConfiguration.getInstance().url("cas.tickets");
    return new RestCasClient(
        new CasConfig.CasConfigBuilder(
                username, password, ticketsUrl, service, CSRF_VALUE, CALLER_ID, "")
            .setJsessionName("JSESSIONID")
            .build());
  }

  @Profile({"default", "dev"})
  @Bean(name = "OppijantunnistusCasClient")
  public RestCasClient getOppijantunnistusCasClient(
      @Value("${cas.service.oppijan-tunnistus}") String service,
      @Value("${valintalaskentakoostepalvelu.app.username.to.valintatieto}") String username,
      @Value("${valintalaskentakoostepalvelu.app.password.to.valintatieto}") String password) {
    String ticketsUrl = UrlConfiguration.getInstance().url("cas.tickets");
    return new RestCasClient(
        new CasConfig.CasConfigBuilder(
                username, password, ticketsUrl, service, CSRF_VALUE, CALLER_ID, "")
            .setJsessionName("ring-session")
            .build());
  }

  @Profile({"default", "dev"})
  @Bean(name = "SeurantaCasClient")
  public RestCasClient getSeurantaCasClient(
      @Value("${cas.service.seuranta}") String service,
      @Value("${valintalaskentakoostepalvelu.app.username.to.valintatieto}") String username,
      @Value("${valintalaskentakoostepalvelu.app.password.to.valintatieto}") String password) {
    String ticketsUrl = UrlConfiguration.getInstance().url("cas.tickets");
    return new RestCasClient(
        new CasConfig.CasConfigBuilder(
                username, password, ticketsUrl, service, CSRF_VALUE, CALLER_ID, "")
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

  @Bean(name = "KoskiHttpClient")
  public HttpClient getKoskiHttpClient(CookieManager cookieManager) {
    return new HttpClient(
        defaultHttpClientBuilder(cookieManager).build(), DateDeserializer.gsonBuilder().create());
  }
}
