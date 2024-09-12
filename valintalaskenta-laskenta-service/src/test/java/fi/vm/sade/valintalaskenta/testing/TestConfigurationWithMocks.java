package fi.vm.sade.valintalaskenta.testing;

import fi.vm.sade.javautils.opintopolku_spring_security.Authorizer;
import fi.vm.sade.valinta.dokumenttipalvelu.Dokumenttipalvelu;
import fi.vm.sade.valintalaskenta.laskenta.config.SwaggerConfiguration;
import fi.vm.sade.valintalaskenta.laskenta.resource.ValintalaskentaResourceImpl;
import fi.vm.sade.valintalaskenta.laskenta.resource.external.ErillisSijoitteluResource;
import fi.vm.sade.valintalaskenta.laskenta.resource.external.ValiSijoitteluResource;
import fi.vm.sade.valintalaskenta.laskenta.resource.external.ValintaperusteetValintatapajonoResource;
import fi.vm.sade.valintalaskenta.laskenta.service.ValintalaskentaService;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.ValisijoitteluKasittelija;
import fi.vm.sade.valintalaskenta.tulos.logging.LaskentaAuditLog;
import fi.vm.sade.valintalaskenta.tulos.logging.LaskentaAuditLogMock;
import io.swagger.v3.oas.models.OpenAPI;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Primary;
import org.springframework.context.annotation.Profile;
import org.springframework.security.cas.authentication.CasAuthenticationProvider;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.web.SecurityFilterChain;

@Profile("test")
@TestConfiguration
class TestConfigurationWithMocks {
  @Bean
  public SecurityFilterChain filterChain(HttpSecurity http) throws Exception {
    http.headers().disable().csrf().disable().authorizeHttpRequests().anyRequest().permitAll();
    return http.build();
  }

  @Bean
  public Authorizer authorizer() {
    return Mockito.mock(Authorizer.class);
  }

  @Bean
  public CasAuthenticationProvider casAuthenticationProvider() {
    return Mockito.mock(CasAuthenticationProvider.class);
  }

  @Bean("valintalaskentaResourceImpl")
  public ValintalaskentaResourceImpl valintalaskentaResourceImpl(
      final ValintalaskentaService valintalaskentaService,
      final ValisijoitteluKasittelija valisijoitteluKasittelija,
      @Qualifier("mockValiSijoitteluResource") final ValiSijoitteluResource valiSijoitteluResource,
      @Qualifier("mockErillisSijoitteluResource")
          final ErillisSijoitteluResource erillisSijoitteluResource,
      @Qualifier("mockValintaperusteetValintatapajonoResource")
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
  public OpenAPI openAPI() {
    return new SwaggerConfiguration().valintalaskentaAPI();
  }

  @Bean
  public ApplicationContextGetter applicationContextGetter() {
    return new ApplicationContextGetter();
  }

  @Primary
  @Bean
  public ValintalaskentaService mockValintalaskentaService() {
    return Mockito.mock(ValintalaskentaService.class);
  }

  @Primary
  @Bean
  public ValiSijoitteluResource mockValiSijoitteluResource() {
    return Mockito.mock(ValiSijoitteluResource.class);
  }

  @Primary
  @Bean
  public ErillisSijoitteluResource mockErillisSijoitteluResource() {
    return Mockito.mock(ErillisSijoitteluResource.class);
  }

  @Primary
  @Bean
  public ValintaperusteetValintatapajonoResource mockValintaperusteetValintatapajonoResource() {
    return Mockito.mock(ValintaperusteetValintatapajonoResource.class);
  }

  @Primary
  @Bean
  public ValisijoitteluKasittelija mockValisijoitteluKasittelija() {
    return Mockito.mock(ValisijoitteluKasittelija.class);
  }

  @Primary
  @Bean
  public LaskentaAuditLog laskentaAuditLogMock() {
    return new LaskentaAuditLogMock();
  }

  @Primary
  @Bean
  public Dokumenttipalvelu dokumenttipalvelu() {
    return Mockito.mock(Dokumenttipalvelu.class);
  }
}
