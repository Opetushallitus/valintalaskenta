package fi.vm.sade.valintalaskenta.laskenta;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.web.embedded.tomcat.TomcatServletWebServerFactory;
import org.springframework.boot.web.server.WebServerFactoryCustomizer;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;

@SpringBootApplication
@EnableWebMvc
public class App {
  public static final String CONTEXT_PATH = "/valintalaskenta-laskenta-service";

  @Component
  public static class CustomContainer
      implements WebServerFactoryCustomizer<TomcatServletWebServerFactory> {
    @Override
    public void customize(final TomcatServletWebServerFactory factory) {
      factory.setContextPath(CONTEXT_PATH);
      factory.setPort(8080);
    }
  }

  public static void main(String[] args) {
    System.setProperty("server.servlet.context-path", CONTEXT_PATH);
    SpringApplication.run(App.class, args);
  }
}
