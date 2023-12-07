package fi.vm.sade.valintalaskenta.testing;

import fi.vm.sade.valinta.dokumenttipalvelu.Dokumenttipalvelu;
import org.mockito.Mockito;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Primary;
import org.springframework.context.annotation.Profile;

@Profile("test-mockless")
@TestConfiguration
class TestConfigurationWithoutMocks {
  @Bean
  public ApplicationContextGetter applicationContextGetter() {
    return new ApplicationContextGetter();
  }

  @Primary
  @Bean
  public Dokumenttipalvelu dokumenttipalvelu() {
    return Mockito.mock(Dokumenttipalvelu.class);
  }
}
