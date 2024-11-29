package fi.vm.sade.valintalaskenta.laskenta;

import static org.testcontainers.containers.localstack.LocalStackContainer.Service.CLOUDWATCH;

import com.github.dockerjava.api.model.ExposedPort;
import com.github.dockerjava.api.model.HostConfig;
import com.github.dockerjava.api.model.PortBinding;
import com.github.dockerjava.api.model.Ports;
import fi.vm.sade.valintalaskenta.App;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.SpringApplication;
import org.springframework.context.annotation.Profile;
import org.springframework.test.util.TestSocketUtils;
import org.testcontainers.containers.localstack.LocalStackContainer;
import org.testcontainers.utility.DockerImageName;

@Profile("dev")
public class DevApp {

  private static final Logger LOG = LoggerFactory.getLogger(DevApp.class);

  private static final String ENVIRONMENT = "testi";

  private static final int localstackPort = TestSocketUtils.findAvailableTcpPort();

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
                                  Ports.Binding.bindPort(localstackPort), new ExposedPort(4566)))));

  public static void main(String[] args) {
    localStackContainer.start();

    System.setProperty("localstackPort", localstackPort + "");
    System.setProperty("aws.accessKeyId", "localstack");
    System.setProperty("aws.secretAccessKey", "localstack");

    System.setProperty("spring.profiles.active", "dev");

    System.setProperty(
        "valintalaskenta-laskenta-service.postgresql.url",
        "jdbc:tc:postgresql:15.4:///test_database");
    System.setProperty("valintalaskenta-laskenta-service.postgresql.user", "user");
    System.setProperty("valintalaskenta-laskenta-service.postgresql.password", "password");
    System.setProperty("valintalaskenta-laskenta-service.postgresql.password", "password");
    System.setProperty("valintalaskenta-laskenta-service.postgresql.maxactive", "150");

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
