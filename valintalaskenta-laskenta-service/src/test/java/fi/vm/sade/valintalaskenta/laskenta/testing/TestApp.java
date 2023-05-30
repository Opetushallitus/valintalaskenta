package fi.vm.sade.valintalaskenta.laskenta.testing;

import static java.lang.Integer.parseInt;

import fi.vm.sade.integrationtest.util.PortChecker;
import fi.vm.sade.javautils.opintopolku_spring_security.Authorizer;
import fi.vm.sade.valintalaskenta.laskenta.App;
import fi.vm.sade.valintalaskenta.laskenta.config.SwaggerConfiguration;
import fi.vm.sade.valintalaskenta.laskenta.resource.ValintalaskentaPaloissaResourceImpl;
import fi.vm.sade.valintalaskenta.laskenta.resource.ValintalaskentaResourceImpl;
import fi.vm.sade.valintalaskenta.laskenta.resource.external.ErillisSijoitteluResource;
import fi.vm.sade.valintalaskenta.laskenta.resource.external.ValiSijoitteluResource;
import fi.vm.sade.valintalaskenta.laskenta.resource.external.ValintaperusteetValintatapajonoResource;
import fi.vm.sade.valintalaskenta.laskenta.service.ValintalaskentaService;
import fi.vm.sade.valintalaskenta.laskenta.service.valinta.impl.ValisijoitteluKasittelija;
import io.swagger.v3.oas.models.OpenAPI;
import org.mockito.Mockito;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.web.embedded.tomcat.TomcatServletWebServerFactory;
import org.springframework.boot.web.server.WebServerFactoryCustomizer;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ImportResource;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;

@SpringBootApplication
@EnableWebMvc
@ImportResource("classpath:application-context-http-test.xml")
public class TestApp {
  public static final int PORT = portFromEnvOrRandom();
  public static final String ROOT_URL =
      String.format("http://localhost:%d/valintalaskenta-laskenta-service/resources", PORT);

  @Bean
  public Authorizer authorizer() {
    return Mockito.mock(Authorizer.class);
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
      factory.setPort(PORT);
    }
  }

  public static int portFromEnvOrRandom() {
    final String port = System.getProperty("TestApp.server.port");
    if (port != null) {
      return parseInt(port);
    }
    return PortChecker.findFreeLocalPort();
  }

  public static void startTestApp() {
    System.setProperty("TestApp.server.rootUrl", ROOT_URL);
    SpringApplication.run(TestApp.class);
  }

  public static void stopTestApp() {
    if (ApplicationContextGetter.applicationContext != null) {
      ((ConfigurableApplicationContext) ApplicationContextGetter.applicationContext).close();
    }
  }

  public static void main(String[] args) {
    System.setProperty("TestApp.server.port", "9080");
    TestApp.startTestApp();
  }

  public static class ApplicationContextGetter implements ApplicationContextAware {
    public static ApplicationContext applicationContext;

    @Override
    public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
      ApplicationContextGetter.applicationContext = applicationContext;
    }
  }
}
