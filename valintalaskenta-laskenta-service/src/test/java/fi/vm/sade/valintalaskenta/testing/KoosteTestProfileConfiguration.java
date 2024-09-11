package fi.vm.sade.valintalaskenta.testing;

import fi.vm.sade.valinta.kooste.external.resource.viestintapalvelu.RestCasClient;
import java.io.IOException;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicReference;
import javax.servlet.*;
import org.apache.cxf.interceptor.Fault;
import org.apache.cxf.message.Message;
import org.apache.cxf.phase.AbstractPhaseInterceptor;
import org.apache.cxf.phase.Phase;
import org.asynchttpclient.AsyncHttpClient;
import org.asynchttpclient.Dsl;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class KoosteTestProfileConfiguration {
  private static final Logger LOG = LoggerFactory.getLogger(KoosteTestProfileConfiguration.class);

  public static AtomicReference<String> PROXY_SERVER = new AtomicReference<>();

  @Bean(name = "testProps")
  public static org.springframework.context.support.PropertySourcesPlaceholderConfigurer
      getPropertyPlaceholderConfigurer() {
    final String proxyServer = PROXY_SERVER.get();
    Properties p0 = new Properties();

    p0.setProperty(
        "valintalaskentakoostepalvelu.jatkuvasijoittelu.timer",
        "time=2018-12-12 10:12:12&delay=10000000");
    p0.setProperty(
        "valintalaskentakoostepalvelu.valintalaskenta.rest.url",
        "http://" + proxyServer + "/valintalaskenta-laskenta-service/resources");
    p0.setProperty(
        "valintalaskentakoostepalvelu.viestintapalvelu.url",
        "http://" + proxyServer + "/viestintapalvelu");
    p0.setProperty(
        "valintalaskentakoostepalvelu.hakemus.rest.url", "http://" + proxyServer + "/haku-app");
    p0.setProperty("valintalaskentakoostepalvelu.koodiService.url", "http://localhost");
    p0.setProperty("cas.callback.valintalaskentakoostepalvelu", "http://localhost");
    p0.setProperty("valintalaskentakoostepalvelu.dokumenttipalvelu.rest.url", "http://localhost");
    p0.setProperty("aws.region", "eu-west-1");
    p0.setProperty("aws.bucket.name", "opintopolku-local-dokumenttipalvelu");

    p0.setProperty("valintalaskentakoostepalvelu.koski.username", "koostepalvelu2koski");
    p0.setProperty("valintalaskentakoostepalvelu.koski.password", "secret");
    p0.setProperty("valintalaskentakoostepalvelu.koski.max.oppijat.post.size", "1000");
    p0.setProperty(
        "valintalaskentakoostepalvelu.laskenta.funktionimet.joille.haetaan.tiedot.koskesta",
        "HAEAMMATILLINENYTOARVOSANA,HAEAMMATILLINENYTOARVIOINTIASTEIKKO,ITEROIAMMATILLISETTUTKINNOT,ITEROIAMMATILLISETOSAT,ITEROIAMMATILLISETYTOOSAALUEET,HAEAMMATILLISENOSANLAAJUUS,HAEAMMATILLISENOSANARVOSANA,HAEAMMATILLISENYTOOSAALUEENLAAJUUS,HAEAMMATILLISENYTOOSAALUEENARVOSANA,HAEAMMATILLISENTUTKINNONKESKIARVO,HAEAMMATILLISENTUTKINNONSUORITUSTAPA");
    p0.setProperty(
        "valintalaskentakoostepalvelu.laskenta.opiskeluoikeustyypit.joille.haetaan.tiedot.koskesta",
        "ammatillinenkoulutus");
    p0.setProperty(
        "valintalaskentakoostepalvelu.laskenta.kosken.historiapyyntojen.rinnakkaisuus", "9");

    p0.setProperty("valintalaskentakoostepalvelu.seuranta.rest.url", "http://localhost");
    p0.setProperty(
        "valintalaskentakoostepalvelu.organisaatioService.rest.url",
        "http://" + proxyServer + "/organisaatio-service/rest");
    p0.setProperty(
        "valintalaskentakoostepalvelu.organisaatio-service-url",
        "http://" + proxyServer + "/organisaatio-service");
    p0.setProperty(
        "valintalaskentakoostepalvelu.tarjonta.rest.url",
        "http://" + proxyServer + "/tarjonta-service/rest");
    p0.setProperty(
        "valintalaskentakoostepalvelu.koodisto.url", "https://itest-virkailija.oph.ware.fi/");
    p0.setProperty("valintalaskentakoostepalvelu.tarjontaService.url", "http://localhost");
    p0.setProperty("valintalaskentakoostepalvelu.kirjeet.polling.interval.millis", "50");
    p0.setProperty("root.organisaatio.oid", "");
    p0.setProperty("kela.ftp.protocol", "ftp");
    p0.setProperty("kela.ftp.username", "username");
    p0.setProperty("kela.ftp.password", "password");
    p0.setProperty("kela.ftp.parameters", "");
    p0.setProperty("kela.ftp.host", "host");
    p0.setProperty("kela.ftp.port", "22");
    p0.setProperty("kela.ftp.path", "/");

    p0.setProperty("host.ilb", "http://" + proxyServer);

    p0.setProperty("web.url.cas", "http://localhost");
    p0.setProperty("cas.service.viestintapalvelu", "");
    p0.setProperty("cas.service.sijoittelu-service", "");
    p0.setProperty("cas.service.organisaatio-service", "");
    p0.setProperty("cas.service.valintalaskenta-service", "");
    p0.setProperty("cas.service.valintaperusteet-service", "");
    p0.setProperty("cas.service.valintapiste-service", "");
    p0.setProperty(
        "valintalaskentakoostepalvelu.swagger.basepath", "/valintalaskentakoostepalvelu/resources");
    p0.setProperty("host.scheme", "http");
    p0.setProperty("host.virkailija", "" + proxyServer);
    p0.setProperty("cas.service.valintalaskentakoostepalvelu", "");
    p0.setProperty("cas.service.haku-service", "");
    p0.setProperty("cas.service.authentication-service", "");
    p0.setProperty("cas.service.oppijanumerorekisteri-service", "");
    p0.setProperty("valintalaskentakoostepalvelu.app.username.to.sijoittelu", "");
    p0.setProperty("valintalaskentakoostepalvelu.app.password.to.sijoittelu", "");
    p0.setProperty("valintalaskentakoostepalvelu.app.username.to.valintatieto", "");
    p0.setProperty("valintalaskentakoostepalvelu.app.password.to.valintatieto", "");
    p0.setProperty("valintalaskentakoostepalvelu.app.username.to.haku", "");
    p0.setProperty("valintalaskentakoostepalvelu.app.password.to.haku", "");
    p0.setProperty("valintalaskentakoostepalvelu.app.username.to.valintaperusteet", "");
    p0.setProperty("valintalaskentakoostepalvelu.app.password.to.valintaperusteet", "");
    p0.setProperty("valintalaskentakoostepalvelu.maxWorkerCount", "0");

    p0.setProperty("omatsivut.email.application.modify.link.en", "https://en.test.domain/token/");
    p0.setProperty("omatsivut.email.application.modify.link.fi", "https://fi.test.domain/token/");
    p0.setProperty("omatsivut.email.application.modify.link.sv", "https://sv.test.domain/token/");

    p0.setProperty("valintalaskentakoostepalvelu.tarjonta.sync.cron", "0 0 0 * * SUN-SAT");

    LOG.info(String.format("Lisätään testiajoa varten propertyjä: %s", p0));

    org.springframework.context.support.PropertySourcesPlaceholderConfigurer defaultProps =
        new org.springframework.context.support.PropertySourcesPlaceholderConfigurer();
    defaultProps.setProperties(p0);
    defaultProps.setOrder(0);
    defaultProps.setLocalOverride(true);
    return defaultProps;
  }

  private static final AbstractPhaseInterceptor<Message> INTERCEPTOR =
      new AbstractPhaseInterceptor<Message>(Phase.PRE_PROTOCOL) {
        @Override
        public void handleMessage(Message message) throws Fault {}
      };

  private static final AsyncHttpClient asyncHttpClient = Dsl.asyncHttpClient();
  private static RestCasClient REST_CAS_CLIENT =
      new RestCasClient(
          request -> asyncHttpClient.executeRequest(request).toCompletableFuture()) {};

  @Bean(name = "AtaruCasClient")
  public RestCasClient getAtaruCasClient() {
    return REST_CAS_CLIENT;
  }

  @Bean(name = "HakuAppCasClient")
  public RestCasClient getHakuAppCasClient() {
    return REST_CAS_CLIENT;
  }

  @Bean(name = "SijoitteluServiceCasClient")
  public RestCasClient getSijoitteluServiceCasClient() {
    return REST_CAS_CLIENT;
  }

  @Bean(name = "HakukohderyhmapalveluCasClient")
  public RestCasClient getHakukohderyhmapalveluCasClient() {
    return REST_CAS_CLIENT;
  }

  @Bean(name = "KoutaCasClient")
  public RestCasClient getKoutaCasClient() {
    return REST_CAS_CLIENT;
  }

  @Bean(name = "ViestintapalveluCasClient")
  public RestCasClient getViestintapalveluCasClient() {
    return REST_CAS_CLIENT;
  }

  @Bean(name = "OppijanumerorekisteriCasClient")
  public RestCasClient getOppijanumerorekisteriCasClient() {
    return REST_CAS_CLIENT;
  }

  @Bean(name = "ValintapisteServiceCasClient")
  public RestCasClient getValintapisteServiceCasClient() {
    return REST_CAS_CLIENT;
  }

  @Bean(name = "SuoritusrekisteriCasClient")
  public RestCasClient getSuoritusrekisteriCasClient() {
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

  @Bean(name = "ryhmasahkopostiCasClient")
  public RestCasClient getRyhmasahkopostiCasClient() {
    return REST_CAS_CLIENT;
  }

  @Bean(name = "OppijantunnistusCasClient")
  public RestCasClient getOppijantunnistusCasClient() {
    return REST_CAS_CLIENT;
  }

  @Bean(name = "SeurantaCasClient")
  public RestCasClient getSeurantaCasClient() {
    return REST_CAS_CLIENT;
  }

  @Bean(name = "springSecurityFilterChain")
  public static Filter getFilter() {
    return new Filter() {
      @Override
      public void init(FilterConfig filterConfig) {}

      @Override
      public void doFilter(
          ServletRequest servletRequest, ServletResponse servletResponse, FilterChain filterChain)
          throws IOException, ServletException {
        filterChain.doFilter(servletRequest, servletResponse);
      }

      @Override
      public void destroy() {}
    };
  }

  @Bean(name = "viestintapalveluClientCasInterceptor")
  public AbstractPhaseInterceptor<Message> getViestintapalveluClientCasInterceptor() {
    return INTERCEPTOR;
  }

  @Bean(name = "OppijanumerorekisteriServiceRestClientCasInterceptor")
  public AbstractPhaseInterceptor<Message>
      getOppijanumerorekisteriServiceRestClientCasInterceptor() {
    return INTERCEPTOR;
  }
}
