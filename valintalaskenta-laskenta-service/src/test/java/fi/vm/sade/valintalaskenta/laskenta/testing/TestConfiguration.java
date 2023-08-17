package fi.vm.sade.valintalaskenta.laskenta.testing;

import static java.lang.Integer.parseInt;

import com.fasterxml.jackson.jaxrs.json.JacksonJsonProvider;
import com.mongodb.MongoClient;
import fi.vm.sade.javautils.opintopolku_spring_security.Authorizer;
import fi.vm.sade.valinta.sharedutils.http.CxfExceptionLogger;
import fi.vm.sade.valintalaskenta.laskenta.App;
import fi.vm.sade.valintalaskenta.laskenta.ObjectMapperProvider;
import fi.vm.sade.valintalaskenta.laskenta.config.MongoConfiguration;
import fi.vm.sade.valintalaskenta.laskenta.config.SwaggerConfiguration;
import fi.vm.sade.valintalaskenta.laskenta.resource.JaxrsConfiguration;
import fi.vm.sade.valintalaskenta.laskenta.resource.ValintalaskentaPaloissaResourceImpl;
import fi.vm.sade.valintalaskenta.laskenta.resource.ValintalaskentaResourceImpl;
import fi.vm.sade.valintalaskenta.laskenta.resource.external.ErillisSijoitteluResource;
import fi.vm.sade.valintalaskenta.laskenta.resource.external.ValiSijoitteluResource;
import fi.vm.sade.valintalaskenta.laskenta.resource.external.ValintaperusteetValintatapajonoResource;
import fi.vm.sade.valintalaskenta.laskenta.service.ValintalaskentaService;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.ValisijoitteluKasittelija;
import fi.vm.sade.valintalaskenta.tulos.logging.LaskentaAuditLogMock;
import io.swagger.v3.oas.models.OpenAPI;
import org.mockito.Mockito;
import org.mongodb.morphia.Datastore;
import org.mongodb.morphia.Morphia;
import org.mongodb.morphia.mapping.DefaultCreator;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.web.embedded.tomcat.TomcatServletWebServerFactory;
import org.springframework.boot.web.server.WebServerFactoryCustomizer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.cas.authentication.CasAuthenticationProvider;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
import org.springframework.stereotype.Component;

@Configuration
public class TestConfiguration extends WebSecurityConfigurerAdapter {

  @Override
  protected void configure(HttpSecurity http) throws Exception {
    http.headers().disable().csrf().disable().authorizeHttpRequests().anyRequest().permitAll();
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
  public ValintalaskentaPaloissaResourceImpl valintalaskentaPaloissaResource(
      ValintalaskentaResourceImpl valintalaskentaResource) {
    return new ValintalaskentaPaloissaResourceImpl(valintalaskentaResource);
  }

  @Bean
  public OpenAPI openAPI() {
    return new SwaggerConfiguration().valintalaskentaAPI();
  }

  @Component
  public static class CustomContainer
      implements WebServerFactoryCustomizer<TomcatServletWebServerFactory> {
    @Override
    public void customize(TomcatServletWebServerFactory factory) {
      factory.setContextPath(App.CONTEXT_PATH);
      factory.setPort(parseInt(System.getProperty("TestApp.server.port")));
    }
  }

  @Bean
  public TestApp.ApplicationContextGetter applicationContextGetter() {
    return new TestApp.ApplicationContextGetter();
  }

  @Bean
  public ValiSijoitteluResource mockValiSijoitteluResource() {
    return Mockito.mock(ValiSijoitteluResource.class);
  }

  @Bean
  public ErillisSijoitteluResource mockErillisSijoitteluResource() {
    return Mockito.mock(ErillisSijoitteluResource.class);
  }

  @Bean
  public ValintaperusteetValintatapajonoResource mockValintaperusteetValintatapajonoResource() {
    return Mockito.mock(ValintaperusteetValintatapajonoResource.class);
  }

  @Bean
  public ValintalaskentaService mockValintalaskentaService() {
    return Mockito.mock(ValintalaskentaService.class);
  }

  @Bean
  public ValisijoitteluKasittelija mockValisijoitteluKasittelija() {
    return Mockito.mock(ValisijoitteluKasittelija.class);
  }

  @Bean
  public LaskentaAuditLogMock laskentaAuditLogMock() {
    return new LaskentaAuditLogMock();
  }

  @Bean(name = "mongodFactory", destroyMethod = "shutdown")
  public ValintalaskentaMongodForTestsFactory mongodFactory() {
    return new ValintalaskentaMongodForTestsFactory();
  }

  @Bean(name = "mongo")
  public MongoClient mongo(
      @Qualifier("mongodFactory") final ValintalaskentaMongodForTestsFactory mongodFactory) {
    return mongodFactory.newMongo();
  }

  @Bean(name = "morphia")
  public Morphia morphia() {
    Morphia morphia = new Morphia();
    morphia
        .getMapper()
        .getOptions()
        .setObjectFactory(
            new DefaultCreator() {
              @Override
              protected ClassLoader getClassLoaderForClass() {
                return MongoConfiguration.class.getClassLoader();
              }
            });
    return morphia;
  }

  @Bean(name = "datastore2")
  public Datastore datastore2(
      @Qualifier("morphia") final Morphia morphia, @Qualifier("mongo") final MongoClient mongo) {
    return morphia.createDatastore(mongo, "test");
  }

  @Bean
  public CxfExceptionLogger cxfExceptionLogger() {
    return new CxfExceptionLogger();
  }

  @Bean
  public JaxrsConfiguration jaxrsConfiguration() {
    return new JaxrsConfiguration();
  }

  @Bean
  public JacksonJsonProvider jacksonJsonProvider() {
    return new JacksonJsonProvider();
  }

  @Bean
  public ObjectMapperProvider objectMapperProvider() {
    return new ObjectMapperProvider();
  }
}
