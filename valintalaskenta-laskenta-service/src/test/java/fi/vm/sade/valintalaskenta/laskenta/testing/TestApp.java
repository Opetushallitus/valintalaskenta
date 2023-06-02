package fi.vm.sade.valintalaskenta.laskenta.testing;

import fi.vm.sade.integrationtest.util.PortChecker;
import org.springframework.beans.BeansException;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;

@SpringBootApplication
@EnableWebMvc
@EnableWebSecurity(debug = true)
public class TestApp {
  public static void startTestApp() {
    startTestApp(PortChecker.findFreeLocalPort());
  }

  private static void startTestApp(final int port) {
    System.setProperty("TestApp.server.port", String.valueOf(port));
    final String rootUrl = String
        .format("http://localhost:%d/valintalaskenta-laskenta-service/resources", port);
    System.setProperty("TestApp.server.rootUrl", rootUrl);
    SpringApplication.run(TestApp.class);
  }

  public static void stopTestApp() {
    if (ApplicationContextGetter.applicationContext != null) {
      ((ConfigurableApplicationContext) ApplicationContextGetter.applicationContext).close();
    }
  }

  public static void main(String[] args) {
    startTestApp(9080);
  }

  public static class ApplicationContextGetter implements ApplicationContextAware {
    public static ApplicationContext applicationContext;

    @Override
    public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
      ApplicationContextGetter.applicationContext = applicationContext;
    }
  }
}
