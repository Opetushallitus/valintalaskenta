package fi.vm.sade.valintalaskenta.ovara.test.config;

import fi.vm.sade.valintalaskenta.ovara.ajastus.repository.SiirtotiedostoProsessiRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Primary;
import org.springframework.context.annotation.Profile;
import org.springframework.data.jdbc.repository.config.EnableJdbcRepositories;

@Profile("test")
@EnableJdbcRepositories(repositoryBaseClass = SiirtotiedostoProsessiRepository.class)
@TestConfiguration
public class OvaraTestConfigurationWithMocks {

  @Autowired SiirtotiedostoProsessiRepository siirtotiedostoProsessiRepository;

  @Primary
  @Bean
  public SiirtotiedostoProsessiRepository siirtotiedostoProsessiRepository() {
    return siirtotiedostoProsessiRepository;
  }
}
