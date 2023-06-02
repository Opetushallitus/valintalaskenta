package fi.vm.sade.valintalaskenta.laskenta.config;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.jaxrs.json.JacksonJsonProvider;
import fi.vm.sade.javautils.cxf.OphRequestHeadersCxfInterceptor;
import fi.vm.sade.javautils.legacy_cxf_cas.authentication.cas.CasApplicationAsAUserInterceptor;
import fi.vm.sade.valintalaskenta.laskenta.ObjectMapperProvider;
import fi.vm.sade.valintalaskenta.laskenta.resource.ValintalaskentaResourceImpl;
import fi.vm.sade.valintalaskenta.laskenta.resource.external.ErillisSijoitteluResource;
import fi.vm.sade.valintalaskenta.laskenta.resource.external.ValiSijoitteluResource;
import fi.vm.sade.valintalaskenta.laskenta.resource.external.ValintaperusteetValintatapajonoResource;
import fi.vm.sade.valintalaskenta.laskenta.resource.filter.CorsResponseFilter;
import fi.vm.sade.valintalaskenta.laskenta.service.ValintalaskentaService;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.ValisijoitteluKasittelija;
import fi.vm.sade.valintalaskenta.tulos.LaskentaAudit;
import fi.vm.sade.valintalaskenta.tulos.logging.LaskentaAuditLogImpl;
import java.util.List;
import org.apache.cxf.endpoint.Client;
import org.apache.cxf.frontend.ClientProxy;
import org.apache.cxf.jaxrs.client.JAXRSClientFactoryBean;
import org.apache.cxf.message.Message;
import org.apache.cxf.transport.http.HTTPConduit;
import org.apache.cxf.transports.http.configuration.ConnectionType;
import org.apache.cxf.transports.http.configuration.HTTPClientPolicy;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.ImportResource;

@Configuration
@ImportResource("classpath:spring/application-context.xml")
public class ValintaLaskentaLaskentaConfiguration {
  @Bean("valintalaskentaResourceImpl")
  @Autowired
  public ValintalaskentaResourceImpl valintalaskentaResourceImpl(
      final ValintalaskentaService valintalaskentaService,
      final ValisijoitteluKasittelija valisijoitteluKasittelija,
      final ValiSijoitteluResource valiSijoitteluResource,
      final ErillisSijoitteluResource erillisSijoitteluResource,
      final ValintaperusteetValintatapajonoResource valintatapajonoResource,
      @Value("${valintalaskenta-laskenta-service.parallelism:1}") final int parallelismFromConfig) {
    return new ValintalaskentaResourceImpl(
        valintalaskentaService,
        valisijoitteluKasittelija,
        valiSijoitteluResource,
        erillisSijoitteluResource,
        valintatapajonoResource,
        parallelismFromConfig);
  }

  @Bean
  public LaskentaAudit laskentaAudit() {
    return new LaskentaAudit();
  }

  @Bean
  public LaskentaAuditLogImpl LaskentaAuditLog() {
    return new LaskentaAuditLogImpl();
  }

  @Bean
  public ObjectMapper jacksonMapper() {
    return new ObjectMapper();
  }

  @Bean
  public OphRequestHeadersCxfInterceptor<Message> ophRequestHeaders() {
    return new OphRequestHeadersCxfInterceptor<>(
        "1.2.246.562.10.00000000001.valintalaskenta.valintalaskenta-laskenta-service");
  }

  @Bean
  public JacksonJsonProvider jacksonJsonProvider() {
    return new JacksonJsonProvider();
  }

  @Bean
  public ObjectMapperProvider laskentaObjectMapperProvider() {
    return new ObjectMapperProvider();
  }

  private CasApplicationAsAUserInterceptor casInterceptor(
      final String casUrl, final String targetUrl, final String username, final String password) {
    final CasApplicationAsAUserInterceptor interceptor = new CasApplicationAsAUserInterceptor();
    interceptor.setWebCasUrl(casUrl);
    interceptor.setTargetService(targetUrl);
    interceptor.setAppClientUsername(username);
    interceptor.setAppClientPassword(password);
    return interceptor;
  }

  @Bean(name = "sijoitteluServiceCasInterceptor")
  public CasApplicationAsAUserInterceptor sijoitteluServiceCasInterceptor(
      @Value("${web.url.cas}") final String casUrl,
      @Value("${cas.service.sijoittelu-service}/j_spring_cas_security_check")
          final String targetUrl,
      @Value("${valintalaskentakoostepalvelu.app.username.to.sijoittelu}") final String username,
      @Value("${valintalaskentakoostepalvelu.app.password.to.sijoittelu}") final String password) {
    return casInterceptor(casUrl, targetUrl, username, password);
  }

  @Bean(name = "valintaperusteetServiceCasInterceptor")
  public CasApplicationAsAUserInterceptor valintaperusteetServiceCasInterceptor(
      @Value("${web.url.cas}") final String casUrl,
      @Value("${cas.service.valintaperusteet-service}/j_spring_cas_security_check")
          final String targetUrl,
      @Value("${valintalaskentakoostepalvelu.app.username.to.sijoittelu}") final String username,
      @Value("${valintalaskentakoostepalvelu.app.password.to.sijoittelu}") final String password) {
    return casInterceptor(casUrl, targetUrl, username, password);
  }

  @Value("${valintalaskenta-laskenta-service.global.http.connectionTimeoutMillis:59999}")
  private long clientConnectionTimeout;

  @Value("${valintalaskenta-laskenta-service.global.http.receiveTimeoutMillis:1799999}")
  private long clientReceiveTimeout;

  private <T> T restClient(
      final JacksonJsonProvider jacksonJsonProvider,
      final ObjectMapperProvider objectMapperProvider,
      final String address,
      final OphRequestHeadersCxfInterceptor<Message> ophRequestHeaders,
      final CasApplicationAsAUserInterceptor casInterceptor,
      final Class<T> cls) {
    // Create rest client
    JAXRSClientFactoryBean bean = new JAXRSClientFactoryBean();
    bean.setInheritHeaders(true);
    bean.setAddress(address);
    bean.setProviders(List.of(jacksonJsonProvider, objectMapperProvider));
    bean.setOutInterceptors(List.of(ophRequestHeaders, casInterceptor));

    // Set http conduit settings
    HTTPClientPolicy policy = new HTTPClientPolicy();
    policy.setConnection(ConnectionType.KEEP_ALIVE);
    policy.setConnectionTimeout(clientConnectionTimeout);
    policy.setReceiveTimeout(clientReceiveTimeout);
    policy.setAllowChunking(false);
    final T restClient = bean.create(cls);
    Client client = ClientProxy.getClient(restClient);
    HTTPConduit conduit = (HTTPConduit) client.getConduit();
    conduit.setClient(policy);

    return restClient;
  }

  @Bean(name = "sijoitteluRestClient")
  public ValiSijoitteluResource sijoitteluRestClient(
      final JacksonJsonProvider jacksonJsonProvider,
      final ObjectMapperProvider laskentaObjectMapperProvider,
      @Value("${host.ilb}") final String address,
      final OphRequestHeadersCxfInterceptor<Message> ophRequestHeaders,
      @Qualifier("sijoitteluServiceCasInterceptor")
          final CasApplicationAsAUserInterceptor casInterceptor) {
    return restClient(
        jacksonJsonProvider,
        laskentaObjectMapperProvider,
        address,
        ophRequestHeaders,
        casInterceptor,
        ValiSijoitteluResource.class);
  }

  @Bean(name = "erillissijoitteluRestClient")
  public ErillisSijoitteluResource erillissijoitteluRestClient(
      final JacksonJsonProvider jacksonJsonProvider,
      final ObjectMapperProvider laskentaObjectMapperProvider,
      @Value("${valintalaskentakoostepalvelu.sijoittelu.rest.url}") final String address,
      final OphRequestHeadersCxfInterceptor<Message> ophRequestHeaders,
      @Qualifier("sijoitteluServiceCasInterceptor")
          final CasApplicationAsAUserInterceptor casInterceptor) {
    return restClient(
        jacksonJsonProvider,
        laskentaObjectMapperProvider,
        address,
        ophRequestHeaders,
        casInterceptor,
        ErillisSijoitteluResource.class);
  }

  @Bean(name = "valintatapajonoClient")
  public ValintaperusteetValintatapajonoResource valintatapajonoClient(
      final JacksonJsonProvider jacksonJsonProvider,
      final ObjectMapperProvider laskentaObjectMapperProvider,
      @Value("${valintalaskentakoostepalvelu.valintaperusteet.ilb.host}") final String address,
      final OphRequestHeadersCxfInterceptor<Message> ophRequestHeaders,
      @Qualifier("valintaperusteetServiceCasInterceptor")
          final CasApplicationAsAUserInterceptor casInterceptor) {
    return restClient(
        jacksonJsonProvider,
        laskentaObjectMapperProvider,
        address,
        ophRequestHeaders,
        casInterceptor,
        ValintaperusteetValintatapajonoResource.class);
  }

  @Bean
  public CorsResponseFilter corsResponseFilter() {
    return new CorsResponseFilter();
  }
}
