package fi.vm.sade.valintalaskenta.laskenta.testing;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.test.context.ContextConfiguration;

@SpringBootApplication
@ContextConfiguration(locations = "classpath:application-context-test.xml")
public class ValintaLaskentaLaskentaTestApplication {
  public static void main(final String[] args) {
    SpringApplication.run(ValintaLaskentaLaskentaTestApplication.class, args);
  }
}
