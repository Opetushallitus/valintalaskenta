package fi.vm.sade.valintalaskenta.laskenta;

import org.springframework.boot.SpringApplication;
import org.springframework.context.annotation.Profile;

@Profile("dev")
public class DevApp {

  private static final String ENVIRONMENT = "untuva";

  public static void main(String[] args) {
    System.setProperty("spring.profiles.active", "dev");

    System.setProperty(
        "valintalaskenta-laskenta-service.postgresql.url",
        "jdbc:tc:postgresql:15.4:///test_database");
    System.setProperty("valintalaskenta-laskenta-service.postgresql.user", "user");
    System.setProperty("valintalaskenta-laskenta-service.postgresql.password", "password");
    System.setProperty("valintalaskenta-laskenta-service.postgresql.password", "password");
    System.setProperty("valintalaskenta-laskenta-service.postgresql.maxactive", "30");

    System.setProperty(
        "host.virkailija", String.format("virkailija.%sopintopolku.fi", ENVIRONMENT));
    System.setProperty("host.cas", String.format("virkailija.%sopintopolku.fi", ENVIRONMENT));
    System.setProperty("host.alb", "http://localhost:8888");
    System.setProperty(
        "host.host-virkailija", String.format("virkailija.%sopintopolku.fi", ENVIRONMENT));
    System.setProperty("host.host-alb", "http://localhost:8888");

    System.setProperty(
        "cas-service.service",
        String.format(
            "https://virkailija.%sopintopolku.fi/valintalaskenta-laskenta-service", ENVIRONMENT));

    System.setProperty("server.servlet.context-path", App.CONTEXT_PATH);

    SpringApplication.run(App.class, args);
  }
}
