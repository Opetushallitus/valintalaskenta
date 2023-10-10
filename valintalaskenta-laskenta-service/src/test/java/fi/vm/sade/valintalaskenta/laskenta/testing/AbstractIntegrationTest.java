package fi.vm.sade.valintalaskenta.laskenta.testing;

import fi.vm.sade.javautils.opintopolku_spring_security.Authorizer;
import fi.vm.sade.valintalaskenta.laskenta.App;
import fi.vm.sade.valintalaskenta.laskenta.config.SwaggerConfiguration;
import fi.vm.sade.valintalaskenta.laskenta.dao.repository.HakijaryhmaRepository;
import fi.vm.sade.valintalaskenta.laskenta.dao.repository.ValinnanvaiheRepository;
import fi.vm.sade.valintalaskenta.laskenta.dao.repository.ValintakoeOsallistuminenRepository;
import fi.vm.sade.valintalaskenta.laskenta.resource.ValintalaskentaPaloissaResourceImpl;
import fi.vm.sade.valintalaskenta.laskenta.resource.ValintalaskentaResourceImpl;
import fi.vm.sade.valintalaskenta.laskenta.resource.external.ErillisSijoitteluResource;
import fi.vm.sade.valintalaskenta.laskenta.resource.external.ValiSijoitteluResource;
import fi.vm.sade.valintalaskenta.laskenta.resource.external.ValintaperusteetValintatapajonoResource;
import fi.vm.sade.valintalaskenta.laskenta.service.ValintalaskentaService;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.ValisijoitteluKasittelija;
import fi.vm.sade.valintalaskenta.laskenta.testing.DefaultTestConfiguration;
import fi.vm.sade.valintalaskenta.tulos.logging.LaskentaAuditLogMock;
import io.swagger.v3.oas.models.OpenAPI;
import org.junit.Before;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.test.context.TestConfiguration;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.boot.context.properties.ConfigurationPropertiesScan;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.server.LocalServerPort;
import org.springframework.context.annotation.*;
import org.springframework.data.jdbc.repository.config.EnableJdbcRepositories;
import org.springframework.security.cas.authentication.CasAuthenticationProvider;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.TestExecutionListeners;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.support.DirtiesContextTestExecutionListener;
import org.testcontainers.containers.PostgreSQLContainer;
import org.springframework.boot.test.context.TestConfiguration;


//@EnableJdbcRepositories(basePackages = "fi.vm.sade.valintalaskenta.laskenta.dao.repository")
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT,
  classes = {App.class},
  args = {"--add-opens=java.base/java.lang=ALL-UNNAMED"})
//@Import(DefaultTestConfiguration.class)
@ActiveProfiles("test")
//@ContextConfiguration(locations = "classpath:application-context-test.xml")
//@EnableAutoConfiguration(exclude=AbstractMocklessIntegrationTest.class)
@RunWith(SpringJUnit4ClassRunner.class)
/*@TestExecutionListeners(
        listeners = {
                DirtiesContextTestExecutionListener.class
        })*/
//@EnableWebSecurity(debug = true)
//@EnableJpaRepositories(basePackages = "fi.vm.sade.valintalaskenta.laskenta.dao.repository")
//@EntityScan(basePackages = "fi.vm.sade.valintalaskenta.domain.*")
public abstract class AbstractIntegrationTest {

  @Autowired
  protected HakijaryhmaRepository hakijaryhmaRepository;

  @Autowired
  protected ValinnanvaiheRepository valinnanvaiheRepository;

  @Autowired
  protected ValintakoeOsallistuminenRepository valintakoeOsallistuminenRepository;

  @LocalServerPort
  protected Integer port;

  @Before
  public void setUp() {
    valinnanvaiheRepository.deleteAll();
    hakijaryhmaRepository.deleteAll();
    valintakoeOsallistuminenRepository.deleteAll();
  }

  @Profile("test")
  @TestConfiguration
  static class TestConfig {
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

}
