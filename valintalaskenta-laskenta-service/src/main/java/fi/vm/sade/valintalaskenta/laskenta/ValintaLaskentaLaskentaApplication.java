package fi.vm.sade.valintalaskenta.laskenta;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

@SpringBootApplication
public class ValintaLaskentaLaskentaApplication {
  public static final String CONTEXT_PATH = "/valintalaskenta-laskenta-service";

  public static void main(String[] args) {
    System.setProperty("server.servlet.context-path", CONTEXT_PATH);
    SpringApplication.run(ValintaLaskentaLaskentaApplication.class, args);
  }
}
