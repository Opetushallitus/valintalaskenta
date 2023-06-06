package fi.vm.sade.valintalaskenta.tulos;

import static fi.vm.sade.valintalaskenta.tulos.CxfUtil.casInterceptor;
import static fi.vm.sade.valintalaskenta.tulos.CxfUtil.createClient;

import com.fasterxml.jackson.jaxrs.json.JacksonJsonProvider;
import fi.vm.sade.javautils.cxf.OphRequestHeadersCxfInterceptor;
import fi.vm.sade.javautils.legacy_cxf_cas.authentication.cas.CasApplicationAsAUserInterceptor;
import fi.vm.sade.javautils.opintopolku_spring_security.Authorizer;
import fi.vm.sade.javautils.opintopolku_spring_security.OidProvider;
import fi.vm.sade.javautils.opintopolku_spring_security.OrganisationHierarchyAuthorizer;
import fi.vm.sade.javautils.opintopolku_spring_security.ThreadLocalAuthorizer;
import fi.vm.sade.service.valintaperusteet.resource.ValintaperusteetResource;
import fi.vm.sade.valintalaskenta.tulos.mapping.ValintalaskentaModelMapper;
import org.apache.cxf.message.Message;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class ValintalaskentaTulosConfiguration {
  @Bean(name = "oidProvider")
  public OidProvider oidProvider(
      @Value("${valintalaskenta-laskenta-service.organisaatio-service.url}")
          final String organisaatioServiceUrl,
      @Value("${root.organisaatio.oid}") final String rootOrganisaatioOid) {
    return new OidProvider(
        organisaatioServiceUrl,
        rootOrganisaatioOid,
        "1.2.246.562.10.00000000001.valintalaskenta.valintalaskenta-laskenta-service");
  }

  @Bean(name = "organisationHierarchyAuthorizer")
  public OrganisationHierarchyAuthorizer organisationHierarchyAuthorizer() {
    return new OrganisationHierarchyAuthorizer();
  }

  @Bean(name = "authorizer")
  public Authorizer authorizer() {
    return new ThreadLocalAuthorizer();
  }

  @Bean(name = "modelMapper")
  public ValintalaskentaModelMapper modelMapper() {
    return new ValintalaskentaModelMapper();
  }

  @Bean(name = "tulosOphRequestHeaders")
  public OphRequestHeadersCxfInterceptor<Message> tulosOphRequestHeaders() {
    return new OphRequestHeadersCxfInterceptor<>(
        "1.2.246.562.10.00000000001.valintalaskenta.valintalaskenta-tulos-service");
  }

  @Bean(name = "koostepalveluCasInterceptor")
  public CasApplicationAsAUserInterceptor koostepalveluCasInterceptor(
      @Value("${web.url.cas}") final String casUrl,
      @Value("${cas.service.valintaperusteet-service}/j_spring_cas_security_check")
          final String targetUrl,
      @Value("${valintalaskentakoostepalvelu.app.username.to.sijoittelu}") final String username,
      @Value("${valintalaskentakoostepalvelu.app.password.to.sijoittelu}") final String password) {
    return casInterceptor(casUrl, targetUrl, username, password);
  }

  @Bean(name = "valintaperusteetClient")
  public ValintaperusteetResource valintaperusteetClient(
      final JacksonJsonProvider jacksonJsonProvider,
      final ObjectMapperProvider tulosObjectMapperProvider,
      @Value("${valintalaskentakoostepalvelu.valintaperusteet.ilb.url}") final String address,
      @Qualifier("tulosOphRequestHeaders")
          final OphRequestHeadersCxfInterceptor<Message> tulosOphRequestHeaders,
      @Qualifier("koostepalveluCasInterceptor")
          final CasApplicationAsAUserInterceptor casInterceptor) {
    return createClient(
        jacksonJsonProvider,
        tulosObjectMapperProvider,
        address,
        tulosOphRequestHeaders,
        casInterceptor,
        ValintaperusteetResource.class);
  }
}
