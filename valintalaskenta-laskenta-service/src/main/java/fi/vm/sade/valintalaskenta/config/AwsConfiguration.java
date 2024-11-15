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

@Configuration
public class AwsConfiguration {

  @Bean
  public CloudWatchClient cloudWatchClient(Environment environment) throws URISyntaxException {
    boolean isLocal =
        Arrays.stream(environment.getActiveProfiles())
            .anyMatch(
                env ->
                    (env.equalsIgnoreCase("test")
                        || env.equalsIgnoreCase("test-mockless")
                        || env.equalsIgnoreCase("dev")));

    CloudWatchClientBuilder builder =
        CloudWatchClient.builder().credentialsProvider(DefaultCredentialsProvider.create());

    if (isLocal) {
      builder
          .region(Region.US_EAST_1)
          .endpointOverride(new URI("http://localhost:" + System.getProperty("localstackPort")));
    }

    return builder.build();
  }
}
