package fi.vm.sade.valintalaskenta.laskenta;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;
import springfox.documentation.swagger2.annotations.EnableSwagger2;

@SpringBootApplication
@EnableSwagger2
@EnableWebMvc  // springfox swagger 3.0.0 vaatii tämän
public class App {
  public static final String CONTEXT_PATH = "/valintalaskenta-laskenta-service";

  public static void main(String[] args) {
    System.setProperty("server.servlet.context-path", CONTEXT_PATH);
    SpringApplication.run(App.class, args);
  }
}
