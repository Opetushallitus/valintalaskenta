package fi.vm.sade.valintalaskenta.laskenta.config;

import ch.qos.logback.access.tomcat.LogbackValve;
import java.io.File;
import java.io.FileNotFoundException;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.web.embedded.tomcat.TomcatServletWebServerFactory;
import org.springframework.boot.web.server.WebServerFactoryCustomizer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.util.ResourceUtils;

@Configuration
public class AccessLogConfiguration {
  @Bean
  @ConditionalOnProperty(name = "logback.access")
  public WebServerFactoryCustomizer containerCustomizer(
      @Value("${logback.access:}") final String path) throws FileNotFoundException {
    final File file = ResourceUtils.getFile(path);
    return container -> {
      if (container instanceof TomcatServletWebServerFactory) {
        ((TomcatServletWebServerFactory) container)
            .addContextCustomizers(
                context -> {
                  LogbackValve logbackValve = new LogbackValve();
                  logbackValve.setFilename("logback-access.xml");
                  logbackValve.setAsyncSupported(true);
                  context.getPipeline().addValve(logbackValve);
                });
      }
    };
  }
}
