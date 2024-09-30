package fi.vm.sade.valintalaskenta.laskenta.config;

import static fi.vm.sade.valinta.sharedutils.http.HttpResource.CSRF_VALUE;
import static fi.vm.sade.valintalaskenta.laskenta.config.ConfigEnums.CALLER_ID;
import static fi.vm.sade.valintalaskenta.tulos.RestClientUtil.get;
import static fi.vm.sade.valintalaskenta.tulos.RestClientUtil.post;

import com.fasterxml.jackson.databind.SerializationFeature;
import com.google.gson.reflect.TypeToken;
import fi.vm.sade.javautils.nio.cas.CasClient;
import fi.vm.sade.javautils.nio.cas.CasClientBuilder;
import fi.vm.sade.javautils.nio.cas.CasConfig;
import fi.vm.sade.service.valintaperusteet.laskenta.api.LaskentaService;
import fi.vm.sade.service.valintaperusteet.laskenta.api.LaskentaServiceImpl;
import fi.vm.sade.sijoittelu.tulos.dto.HakukohdeDTO;
import fi.vm.sade.valinta.dokumenttipalvelu.Dokumenttipalvelu;
import fi.vm.sade.valinta.kooste.external.resource.UrlConfiguration;
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
import java.util.Map;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.jackson.Jackson2ObjectMapperBuilderCustomizer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

@Configuration
@ComponentScan(
    basePackages = {
      "fi.vm.sade.valintalaskenta.laskenta",
      "fi.vm.sade.valintalaskenta.tulos",
      "fi.vm.sade.valinta.kooste"
    })
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
        valintatapajonoResource);
  }

  @Bean
  public LaskentaAudit laskentaAudit() {
    return new LaskentaAudit();
  }

  @Bean
  public LaskentaAuditLogImpl LaskentaAuditLog() {
    return new LaskentaAuditLogImpl();
  }

  @Bean(name = "sijoitteluCasClient")
  public CasClient sijoitteluCasClient(
      @Value("${web.url.cas}") final String casUrl,
      @Value("${cas.service.sijoittelu-service}") final String targetUrl,
      @Value("${valintalaskentakoostepalvelu.app.username.to.sijoittelu}") final String username,
      @Value("${valintalaskentakoostepalvelu.app.password.to.sijoittelu}") final String password) {
    return CasClientBuilder.build(
        new CasConfig.CasConfigBuilder(
                username,
                password,
                casUrl,
                targetUrl,
                CSRF_VALUE,
                CALLER_ID.value(),
                "/j_spring_cas_security_check")
            .setJsessionName("JSESSIONID")
            .build());
  }

  @Bean(name = "valintaperusteetServiceCasClient")
  public CasClient valintaperusteetServiceCasClient(
      @Value("${web.url.cas}") final String casUrl,
      @Value("${cas.service.valintaperusteet-service}") final String targetUrl,
      @Value("${valintalaskentakoostepalvelu.app.username.to.sijoittelu}") final String username,
      @Value("${valintalaskentakoostepalvelu.app.password.to.sijoittelu}") final String password) {
    return CasClientBuilder.build(
        new CasConfig.CasConfigBuilder(
                username,
                password,
                casUrl,
                targetUrl,
                CSRF_VALUE,
                CALLER_ID.value(),
                "/j_spring_cas_security_check")
            .setJsessionName("JSESSIONID")
            .build());
  }

  @Bean(name = "seurantaCasClient")
  public CasClient seurantaServiceCasClient(
      @Value("${web.url.cas}") final String casUrl,
      @Value("${cas.service.seuranta-service}") final String targetUrl,
      @Value("${valintalaskentakoostepalvelu.app.username.to.sijoittelu}") final String username,
      @Value("${valintalaskentakoostepalvelu.app.password.to.sijoittelu}") final String password) {
    return CasClientBuilder.build(
        new CasConfig.CasConfigBuilder(
                username,
                password,
                casUrl,
                targetUrl,
                CSRF_VALUE,
                CALLER_ID.value(),
                "/j_spring_cas_security_check")
            .setJsessionName("JSESSIONID")
            .build());
  }

  @Value("${valintalaskenta-laskenta-service.global.http.connectionTimeoutMillis:59999}")
  private Integer clientConnectionTimeout;

  @Value("${valintalaskenta-laskenta-service.global.http.receiveTimeoutMillis:1799999}")
  private Integer clientReceiveTimeout;

  @Bean(name = "sijoitteluRestClient")
  public ValiSijoitteluResource sijoitteluRestClient(
      @Qualifier("sijoitteluCasClient") final CasClient sijoitteluCasClient,
      @Value("${valintalaskentakoostepalvelu.sijoittelu.rest.url}")
          final String sijoitteluBaseUrl) {
    return (hakuOid, hakukohteet) -> {
      final TypeToken<List<HakukohdeDTO>> typeToken = new TypeToken<>() {};
      return post(
          sijoitteluCasClient,
          String.format("%s/valisijoittele/%s", sijoitteluBaseUrl, hakuOid),
          typeToken,
          hakukohteet,
          clientConnectionTimeout,
          clientReceiveTimeout);
    };
  }

  @Bean(name = "erillissijoitteluRestClient")
  public ErillisSijoitteluResource erillissijoitteluRestClient(
      @Qualifier("sijoitteluCasClient") final CasClient sijoitteluCasClient,
      @Value("${valintalaskentakoostepalvelu.sijoittelu.rest.url}")
          final String sijoitteluBaseUrl) {
    return (hakuOid, hakukohteet) -> {
      final TypeToken<Long> typeToken = new TypeToken<>() {};
      final Long result =
          post(
              sijoitteluCasClient,
              String.format("%s/erillissijoittele/%s", sijoitteluBaseUrl, hakuOid),
              typeToken,
              hakukohteet,
              clientConnectionTimeout,
              clientReceiveTimeout);
      return result;
    };
  }

  @Bean(name = "valintatapajonoClient")
  public ValintaperusteetValintatapajonoResource valintatapajonoClient(
      @Qualifier("valintaperusteetServiceCasClient") final CasClient valintaperusteetCasClient,
      @Value("${cas.service.valintaperusteet-service}") final String valintaperusteetBaseUrl) {
    return oids -> {
      final TypeToken<Map<String, List<String>>> typeToken = new TypeToken<>() {};
      final Map<String, List<String>> result =
          get(
              valintaperusteetCasClient,
              String.format(
                  "%s/resources/valintalaskentakoostepalvelu/valintatapajono/kopiot",
                  valintaperusteetBaseUrl),
              typeToken,
              Map.of("oid", oids),
              clientConnectionTimeout,
              clientReceiveTimeout);
      return result;
    };
  }

  @Bean
  public CorsResponseFilter corsResponseFilter() {
    return new CorsResponseFilter();
  }

  @Bean
  public LaskentaService laskentaService() {
    return new LaskentaServiceImpl();
  }

  @Bean
  public Jackson2ObjectMapperBuilderCustomizer jsonCustomizer() {
    return builder -> {
      builder.featuresToEnable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);
    };
  }

  @Bean
  public Dokumenttipalvelu dokumenttipalvelu(
      @Value("${aws.region}") final String region,
      @Value("${aws.bucket.name}") final String bucketName) {
    return new Dokumenttipalvelu(region, bucketName);
  }

  @Bean
  public UrlConfiguration getUrlConfiguration() {
    return UrlConfiguration.getInstance();
  }
}
