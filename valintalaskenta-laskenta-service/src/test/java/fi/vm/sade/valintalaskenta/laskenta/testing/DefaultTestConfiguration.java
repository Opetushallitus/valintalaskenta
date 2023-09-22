package fi.vm.sade.valintalaskenta.laskenta.testing;

import static java.lang.Integer.parseInt;

import fi.vm.sade.javautils.opintopolku_spring_security.Authorizer;
import fi.vm.sade.valintalaskenta.laskenta.App;
import fi.vm.sade.valintalaskenta.laskenta.config.SwaggerConfiguration;
import fi.vm.sade.valintalaskenta.laskenta.resource.ValintalaskentaPaloissaResourceImpl;
import fi.vm.sade.valintalaskenta.laskenta.resource.ValintalaskentaResourceImpl;
import fi.vm.sade.valintalaskenta.laskenta.resource.external.ErillisSijoitteluResource;
import fi.vm.sade.valintalaskenta.laskenta.resource.external.ValiSijoitteluResource;
import fi.vm.sade.valintalaskenta.laskenta.resource.external.ValintaperusteetValintatapajonoResource;
import fi.vm.sade.valintalaskenta.laskenta.service.ValintalaskentaService;
import fi.vm.sade.valintalaskenta.laskenta.service.impl.ValintalaskentaServiceImpl;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.ValisijoitteluKasittelija;
import fi.vm.sade.valintalaskenta.laskenta.testing.TestApp;
import fi.vm.sade.valintalaskenta.tulos.logging.LaskentaAuditLogMock;
import io.swagger.v3.oas.models.OpenAPI;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.boot.web.embedded.tomcat.TomcatServletWebServerFactory;
import org.springframework.boot.web.server.WebServerFactoryCustomizer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.data.jdbc.repository.config.AbstractJdbcConfiguration;
import org.springframework.data.jdbc.repository.config.EnableJdbcRepositories;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcOperations;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.jdbc.datasource.DataSourceTransactionManager;
import org.springframework.security.cas.authentication.CasAuthenticationProvider;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.stereotype.Component;
import org.springframework.transaction.TransactionManager;

import javax.sql.DataSource;


@TestConfiguration
public class DefaultTestConfiguration {

  /*@Bean
  NamedParameterJdbcOperations namedParameterJdbcOperations(DataSource dataSource) {
    return new NamedParameterJdbcTemplate(dataSource);
  }

  @Bean
  TransactionManager transactionManager(DataSource dataSource) {
    return new DataSourceTransactionManager(dataSource);
  }*/

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
  
  public ValintalaskentaPaloissaResourceImpl valintalaskentaPaloissaResource(
      ValintalaskentaResourceImpl valintalaskentaResource) {
    return new ValintalaskentaPaloissaResourceImpl(valintalaskentaResource);
  }

  @Bean
  public OpenAPI openAPI() {
    return new SwaggerConfiguration().valintalaskentaAPI();
  }

  /*@Component
  public static class CustomContainer
      implements WebServerFactoryCustomizer<TomcatServletWebServerFactory> {
    @Override
    public void customize(TomcatServletWebServerFactory factory) {
      factory.setContextPath(App.CONTEXT_PATH);
      factory.setPort(parseInt(System.getProperty("TestApp.server.port")));
    }
  }*/

  @Bean
  public TestApp.ApplicationContextGetter applicationContextGetter() {
    return new TestApp.ApplicationContextGetter();
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
  public LaskentaAuditLogMock laskentaAuditLogMock() {
    return new LaskentaAuditLogMock();
  }

}
