package fi.vm.sade.valintalaskenta.laskenta.config;

import static fi.vm.sade.valintalaskenta.tulos.CxfUtil.casInterceptor;
import static fi.vm.sade.valintalaskenta.tulos.CxfUtil.createClient;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.jaxrs.json.JacksonJsonProvider;
import fi.vm.sade.javautils.cxf.OphRequestHeadersCxfInterceptor;
import fi.vm.sade.javautils.legacy_cxf_cas.authentication.cas.CasApplicationAsAUserInterceptor;
import fi.vm.sade.service.valintaperusteet.laskenta.api.LaskentaService;
import fi.vm.sade.service.valintaperusteet.laskenta.api.LaskentaServiceImpl;
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
import org.apache.cxf.message.Message;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

@Configuration
@ComponentScan(
    basePackages = {"fi.vm.sade.valintalaskenta.laskenta", "fi.vm.sade.valintalaskenta.tulos"})
public class ValintalaskentaLaskentaConfiguration {
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

  @Bean(name = "laskentaOphRequestHeaders")
  public OphRequestHeadersCxfInterceptor<Message> ophRequestHeaders() {
    return new OphRequestHeadersCxfInterceptor<>(
            ConfigEnums.CALLER_ID.value());
  }

  @Bean
  public JacksonJsonProvider jacksonJsonProvider() {
    return new JacksonJsonProvider();
  }

  @Bean
  public ObjectMapperProvider laskentaObjectMapperProvider() {
    return new ObjectMapperProvider();
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

  @Bean(name = "sijoitteluRestClient")
  public ValiSijoitteluResource sijoitteluRestClient(
      final JacksonJsonProvider jacksonJsonProvider,
      final ObjectMapperProvider laskentaObjectMapperProvider,
      @Value("${host.ilb}") final String address,
      @Qualifier("laskentaOphRequestHeaders")
          final OphRequestHeadersCxfInterceptor<Message> laskentaOphRequestHeaders,
      @Qualifier("sijoitteluServiceCasInterceptor")
          final CasApplicationAsAUserInterceptor casInterceptor) {
    return createClient(
        jacksonJsonProvider,
        laskentaObjectMapperProvider,
        address,
        laskentaOphRequestHeaders,
        casInterceptor,
        ValiSijoitteluResource.class,
        clientConnectionTimeout,
        clientReceiveTimeout);
  }

  @Bean(name = "erillissijoitteluRestClient")
  public ErillisSijoitteluResource erillissijoitteluRestClient(
      final JacksonJsonProvider jacksonJsonProvider,
      final ObjectMapperProvider laskentaObjectMapperProvider,
      @Value("${valintalaskentakoostepalvelu.sijoittelu.rest.url}") final String address,
      @Qualifier("laskentaOphRequestHeaders")
          final OphRequestHeadersCxfInterceptor<Message> laskentaOphRequestHeaders,
      @Qualifier("sijoitteluServiceCasInterceptor")
          final CasApplicationAsAUserInterceptor casInterceptor) {
    return createClient(
        jacksonJsonProvider,
        laskentaObjectMapperProvider,
        address,
        laskentaOphRequestHeaders,
        casInterceptor,
        ErillisSijoitteluResource.class,
        clientConnectionTimeout,
        clientReceiveTimeout);
  }

  @Bean(name = "valintatapajonoClient")
  public ValintaperusteetValintatapajonoResource valintatapajonoClient(
      final JacksonJsonProvider jacksonJsonProvider,
      final ObjectMapperProvider laskentaObjectMapperProvider,
      @Value("${valintalaskentakoostepalvelu.valintaperusteet.ilb.host}") final String address,
      @Qualifier("laskentaOphRequestHeaders")
          final OphRequestHeadersCxfInterceptor<Message> laskentaOphRequestHeaders,
      @Qualifier("valintaperusteetServiceCasInterceptor")
          final CasApplicationAsAUserInterceptor casInterceptor) {
    return createClient(
        jacksonJsonProvider,
        laskentaObjectMapperProvider,
        address,
        laskentaOphRequestHeaders,
        casInterceptor,
        ValintaperusteetValintatapajonoResource.class,
        clientConnectionTimeout,
        clientReceiveTimeout);
  }

  @Bean
  public CorsResponseFilter corsResponseFilter() {
    return new CorsResponseFilter();
  }

  @Bean
  public LaskentaService laskentaService() {
    return new LaskentaServiceImpl();
  }
}
