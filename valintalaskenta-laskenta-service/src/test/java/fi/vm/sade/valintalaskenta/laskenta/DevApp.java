package fi.vm.sade.valintalaskenta.laskenta;

import static org.testcontainers.containers.localstack.LocalStackContainer.Service.CLOUDWATCH;

import com.github.dockerjava.api.model.ExposedPort;
import com.github.dockerjava.api.model.HostConfig;
import com.github.dockerjava.api.model.PortBinding;
import com.github.dockerjava.api.model.Ports;
import fi.vm.sade.valintalaskenta.App;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.SpringApplication;
import org.springframework.context.annotation.Profile;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.containers.localstack.LocalStackContainer;
import org.testcontainers.utility.DockerImageName;

@Profile("dev")
public class DevApp {

  private static final Logger LOG = LoggerFactory.getLogger(DevApp.class);

  private static final String ENVIRONMENT = "untuva";

  public static final int LOCALSTACKPORT = 54566;

  private static final LocalStackContainer localStackContainer =
      new LocalStackContainer(new DockerImageName("localstack/localstack:2.2.0"))
          .withServices(CLOUDWATCH)
          .withLogConsumer(frame -> LOG.info(frame.getUtf8StringWithoutLineEnding()))
          .withExposedPorts(4566)
          .withCreateContainerCmdModifier(
              m ->
                  m.withHostConfig(
                      new HostConfig()
                          .withPortBindings(
                              new PortBinding(
                                  Ports.Binding.bindPort(LOCALSTACKPORT), new ExposedPort(4566)))));

  private static final PostgreSQLContainer<?> postgres =
      new PostgreSQLContainer<>("postgres:15").withCommand("postgres -c max_connections=250");

  private static void startContainers() {
    localStackContainer.start();

    postgres.setPortBindings(List.of("55432:5432"));
    postgres.start();
  }

  public static void startApplication(String[] args) {
    System.setProperty("localstackPort", LOCALSTACKPORT + "");
    System.setProperty("aws.accessKeyId", "localstack");
    System.setProperty("aws.secretAccessKey", "localstack");

    System.setProperty("spring.profiles.active", "dev");

    System.setProperty(
        "valintalaskenta-laskenta-service.postgresql.url",
        "jdbc:postgresql://localhost:55432/test");
    System.setProperty("valintalaskenta-laskenta-service.postgresql.user", "test");
    System.setProperty("valintalaskenta-laskenta-service.postgresql.password", "test");
    System.setProperty("valintalaskenta-laskenta-service.postgresql.maxactive", "200");
    System.setProperty(
        "valintalaskenta-laskenta-service.postgresql.driver", "org.postgresql.Driver");

    System.setProperty(
        "host.virkailija", String.format("virkailija.%sopintopolku.fi", ENVIRONMENT));
    System.setProperty("host.cas", String.format("virkailija.%sopintopolku.fi", ENVIRONMENT));
    System.setProperty(
        "host.alb", String.format("https://virkailija.%sopintopolku.fi", ENVIRONMENT));
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

  public static void main(String[] args) {
    startContainers();
    startApplication(args);
  }
}
