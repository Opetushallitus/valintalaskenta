package fi.vm.sade.valintalaskenta.tulos;

import static fi.vm.sade.valinta.sharedutils.http.HttpResource.CSRF_VALUE;

import fi.vm.sade.javautils.nio.cas.CasClient;
import fi.vm.sade.javautils.nio.cas.CasClientBuilder;
import fi.vm.sade.javautils.nio.cas.CasConfig;
import fi.vm.sade.javautils.opintopolku_spring_security.Authorizer;
import fi.vm.sade.javautils.opintopolku_spring_security.OidProvider;
import fi.vm.sade.javautils.opintopolku_spring_security.OrganisationHierarchyAuthorizer;
import fi.vm.sade.javautils.opintopolku_spring_security.ThreadLocalAuthorizer;
import fi.vm.sade.valintalaskenta.tulos.mapping.ValintalaskentaModelMapper;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class ValintalaskentaTulosConfiguration {
  public static final String CALLER_ID =
      "1.2.246.562.10.00000000001.valintalaskenta.valintalaskenta-tulos-service";

  @Bean(name = "oidProvider")
  public OidProvider oidProvider(
      @Value("${valintalaskenta-laskenta-service.organisaatio-service.url}")
          final String organisaatioServiceUrl,
      @Value("${root.organisaatio.oid}") final String rootOrganisaatioOid) {
    return new OidProvider(organisaatioServiceUrl, rootOrganisaatioOid, CALLER_ID);
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

  @Bean(name = "valintaperusteetCasClient")
  public CasClient valintaperusteetCasClient(
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
                CALLER_ID,
                "/j_spring_cas_security_check")
            .setJsessionName("JSESSIONID")
            .build());
  }
}
