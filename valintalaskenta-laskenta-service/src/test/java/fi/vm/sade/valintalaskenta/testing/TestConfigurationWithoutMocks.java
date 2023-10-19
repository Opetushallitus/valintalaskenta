package fi.vm.sade.valintalaskenta.testing;

import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Profile;

@Profile("test-mockless")
@TestConfiguration
class TestConfigurationWithoutMocks {
  @Bean
  public ApplicationContextGetter applicationContextGetter() {
    return new ApplicationContextGetter();
  }
}
