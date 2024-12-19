package fi.vm.sade.valintalaskenta.config;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.Arrays;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.Environment;
import software.amazon.awssdk.auth.credentials.DefaultCredentialsProvider;
import software.amazon.awssdk.regions.Region;
import software.amazon.awssdk.services.cloudwatch.CloudWatchClient;
import software.amazon.awssdk.services.cloudwatch.CloudWatchClientBuilder;
import software.amazon.awssdk.services.ecs.EcsClient;
import software.amazon.awssdk.services.ecs.EcsClientBuilder;

@Configuration
public class AwsConfiguration {

  private final boolean isLocal;

  public static boolean isLocal(Environment environment) {
    return Arrays.stream(environment.getActiveProfiles())
        .anyMatch(
            env ->
                (env.equalsIgnoreCase("test")
                    || env.equalsIgnoreCase("test-mockless")
                    || env.equalsIgnoreCase("dev")));
  }

  public AwsConfiguration(Environment environment) {
    this.isLocal = isLocal(environment);
  }

  @Bean
  public CloudWatchClient cloudWatchClient() throws URISyntaxException {

    CloudWatchClientBuilder builder =
        CloudWatchClient.builder().credentialsProvider(DefaultCredentialsProvider.create());

    if (this.isLocal) {
      builder
          .region(Region.US_EAST_1)
          .endpointOverride(new URI("http://localhost:" + System.getProperty("localstackPort")));
    }

    return builder.build();
  }

  @Bean
  public EcsClient ecsClient() throws URISyntaxException {

    EcsClientBuilder builder =
        EcsClient.builder().credentialsProvider(DefaultCredentialsProvider.create());

    if (this.isLocal) {
      builder
          .region(Region.US_EAST_1)
          .endpointOverride(new URI("http://localhost:" + System.getProperty("localstackPort")));
    }

    return builder.build();
  }
}
